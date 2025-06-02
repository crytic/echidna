{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Echidna.SymExec (createSymTx) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT, liftIO)
import Data.ByteString.Lazy qualified as BS
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (toList, fromList)
import GHC.IORef (IORef, readIORef)
import Optics.Core ((.~), (%), (%~))
import EVM.ABI (abiKind, AbiKind(Dynamic), AbiValue(..), AbiType(..), Sig(..), decodeAbiValue)
import EVM.Fetch qualified as Fetch
import EVM (loadContract, resetState, forceLit)
import EVM.Effects (defaultEnv, defaultConfig, Config(..), config)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(..), SolverGroup)
import EVM.SymExec (IterConfig(..), abstractVM, mkCalldata, LoopHeuristic (..), verifyInputs, VeriOpts(..), checkAssertions)
import EVM.Types (abiKeccak, FunctionSelector, Addr, Frame(..), FrameState(..), VMType(..), EType(..), Expr(..), word256Bytes, Block(..), W256, SMTCex(..), ProofResult(..), Prop(..), Query(..))
import qualified EVM.Types (VM(..), Env(..))
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState, runStateT)
import List.Shuffle (shuffleIO)

import Echidna.Solidity (chooseContract)
import Echidna.Types (fromEVM)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.World (World(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock, maxBlockDelay, maxTimeDelay)
import Echidna.Types.Cache (ContractCache, SlotCache)

-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
-- Also takes an optional Tx argument; this is used as the transaction
--   to symbolize. If none is provided, we do full
--   symbolic execution.
--   The Tx argument, if present, must have a .call value of type SolCall.
createSymTx :: (MonadIO m, MonadThrow m, MonadReader Env m) => Maybe Text -> [SolcContract] -> Maybe Tx -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar [Tx])
createSymTx name cs tx vm = do
  mainContract <- chooseContract cs name
  exploreContract mainContract tx vm

suitableForSymExec :: Method -> Bool
suitableForSymExec m = not $ null m.inputs 
  && null (filter (\(_, t) -> abiKind t == Dynamic) m.inputs) 
  && not (T.isInfixOf "_no_symexec" m.name)

filterTarget :: Maybe [Text] -> [FunctionSelector] -> Maybe Tx -> Method -> Bool
filterTarget symExecTargets assertSigs tx method =
  case (symExecTargets, tx) of
    (Just ms, _)                                       -> method.name `elem` ms
    (_,  Just (Tx { call = SolCall (methodName, _) })) -> methodSig `elem` assertSigs && method.name == methodName && suitableForSymExec method
    _                                                  -> {- methodSig `elem` assertSigs &&-} suitableForSymExec method
 where methodSig = abiKeccak $ encodeUtf8 method.methodSignature

exploreContract :: (MonadIO m, MonadThrow m, MonadReader Env m) => SolcContract -> Maybe Tx -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar[Tx])
exploreContract contract tx vm = do
  conf <- asks (.cfg)
  env <- ask
  contractCacheRef <- asks (.fetchContractCache)
  slotCacheRef <- asks (.fetchSlotCache)  
  let
    allMethods = Map.elems contract.abiMap
    filteredMethods = filter (filterTarget conf.campaignConf.symExecTargets env.world.assertSigs tx) allMethods
    methods = filteredMethods
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock)
    defaultSender = fromJust $ fmap (.dst) tx <|> Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = Naive}
  let veriOpts = VeriOpts {iterConf = iterConfig, simp = True, rpcInfo = undefined}
  let runtimeEnv = defaultEnv { config = defaultConfig { maxWidth = 5, maxDepth = maxExplore, maxBufSize = 12, promiseNoReent = True, debug = True, dumpQueries = False, numCexFuzz = 100 } }

  liftIO $ flip runReaderT runtimeEnv $ withSolvers Bitwuzla (fromIntegral conf.campaignConf.symExecNSolvers) 1 timeoutSMT $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      shuffleMethods <- shuffleIO methods
      res <- forM (take 1 shuffleMethods) $ \method -> do
        liftIO $ putStrLn ("Exploring: " ++ T.unpack method.name)
        calldataSym@(cd, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
        let
          fetcher = cachedOracle contractCacheRef slotCacheRef solvers rpcInfo
          dst = conf.solConf.contractAddr
          vmSym = abstractVM calldataSym contract.runtimeCode Nothing False
        vmSym' <- liftIO $ stToIO vmSym
        vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
        let vm' = vmReset & execState (loadContract (LitAddr dst))
                          & vmMakeSymbolic conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay
                          & #constraints %~ (++ constraints ++ (senderContraints conf.solConf.sender))
                          & #state % #callvalue .~ TxValue
                          & #state % #caller .~ SymAddr "caller"
                          & #state % #calldata .~ cd
                          & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
        -- TODO we might want to switch vm's state.baseState value to to AbstractBase eventually.
        -- Doing so might mess up concolic execution.
        (_, models) <- verifyInputs solvers veriOpts fetcher vm' (Just $ checkAssertions [0x1])
        let results = map fst models
        liftIO $ mapM_ print results
        --liftIO $ mapM_ print $ checkResults results
        let txs = mapMaybe (modelToTx dst vm.block.timestamp vm.block.number method conf.solConf.sender defaultSender) results
        --liftIO $ print $ map (runReaderT mempty $ ppTx vm True) txs
        pure txs
      liftIO $ putMVar resultChan $ concat res
      --liftIO $ print "done"
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)

-- | Sets result to Nothing, and sets gas to ()
vmMakeSymbolic :: W256 -> W256 -> EVM.Types.VM Concrete s -> EVM.Types.VM Symbolic s
vmMakeSymbolic maxTimestampDiff maxNumberDiff vm
  = EVM.Types.VM
  { result         = Nothing
  , state          = frameStateMakeSymbolic vm.state
  , frames         = map frameMakeSymbolic vm.frames
  , env            = vm.env
  , block          = blockMakeSymbolic vm.block
  , tx             = vm.tx
  , logs           = vm.logs
  , traces         = vm.traces
  , cache          = vm.cache
  , burned         = ()
  , iterations     = vm.iterations
  , constraints    = addBlockConstrains maxTimestampDiff maxNumberDiff vm.block vm.constraints
  , config         = vm.config
  , forks          = vm.forks
  , currentFork    = vm.currentFork
  , labels         = vm.labels
  , osEnv          = vm.osEnv
  , freshVar       = vm.freshVar
  , exploreDepth   = 0
  }

blockMakeSymbolic :: Block -> Block
blockMakeSymbolic b
  = b {
      timestamp = Var "symbolic_block_timestamp"
    , number = Var "symbolic_block_number"
  }

addBlockConstrains :: W256 -> W256 -> Block -> [Prop] -> [Prop]
addBlockConstrains maxTimestampDiff maxNumberDiff block cs =
  cs ++ [
    PGT (Var "symbolic_block_timestamp") (block.timestamp), PLT (Sub (Var "symbolic_block_timestamp") (block.timestamp)) $ Lit maxTimestampDiff,
    PGT (Var "symbolic_block_number") (block.number), PLT (Sub (Var "symbolic_block_number") (block.number)) $ Lit maxNumberDiff
  ]

senderContraints :: Set Addr -> [Prop]
senderContraints as = [foldr (\a b -> POr b (PEq (SymAddr "caller") (LitAddr a))) (PBool False) $ Set.toList as]

frameStateMakeSymbolic :: FrameState Concrete s -> FrameState Symbolic s
frameStateMakeSymbolic fs
  = FrameState
  { contract     = fs.contract
  , codeContract = fs.codeContract
  , code         = fs.code
  , pc           = fs.pc
  , stack        = fs.stack
  , memory       = fs.memory
  , memorySize   = fs.memorySize
  , calldata     = fs.calldata
  , callvalue    = fs.callvalue
  , caller       = fs.caller
  , gas          = ()
  , returndata   = fs.returndata
  , static       = fs.static
  , overrideCaller = fs.overrideCaller
  , resetCaller  = fs.resetCaller
  }

frameMakeSymbolic :: Frame Concrete s -> Frame Symbolic s
frameMakeSymbolic fr = Frame { context = fr.context, state = frameStateMakeSymbolic fr.state }

modelToTx :: Addr -> Expr EWord -> Expr EWord -> Method -> Set Addr -> Addr -> ProofResult SMTCex String -> Maybe Tx
modelToTx dst oldTimestamp oldNumber method senders fallbackSender result =
  case result of
    Cex cex ->
      let
        args = zipWith grabArg (snd <$> method.inputs) ["arg" <> T.pack (show n) | n <- [1..] :: [Int]]

        grabArg t
          = case t of
              AbiUIntType _ -> grabNormalArg t
              AbiIntType _ -> grabNormalArg t
              AbiBoolType -> grabNormalArg t
              AbiBytesType _ -> grabNormalArg t
              AbiAddressType -> grabAddressArg
              AbiArrayType n mt -> grabArrayArg n mt
              AbiTupleType mt -> grabTupleArg mt
              _ -> error "Unexpected ABI type in `modelToTx`"

        grabNormalArg argType name
          = case Map.lookup (Var name) cex.vars of
              Just w ->
                decodeAbiValue argType (BS.fromStrict (word256Bytes w))
              Nothing -> -- put a placeholder
                decodeAbiValue argType (BS.repeat 0)

        grabAddressArg name = AbiAddress $ fromMaybe 0 $ Map.lookup (SymAddr name) cex.addrs

        grabArrayArg nElem memberType name = AbiArray nElem memberType $ fromList [grabArg memberType $ name <> "-a-" <> T.pack (show n) | n <- [0..nElem] :: [Int]]

        grabTupleArg memberTypes name = AbiTuple $ fromList [grabArg t $ name <> "-t-" <> T.pack (show n) | (n, t) <- zip ([0..] :: [Int]) (toList memberTypes)]

        src_ = fromMaybe 0 $ Map.lookup (SymAddr "caller") cex.addrs
        src = if Set.member src_ senders then src_ else fallbackSender
        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

        newTimestamp = fromMaybe 0 $ Map.lookup (Var "symbolic_block_timestamp") cex.vars
        diffTimestamp = if newTimestamp == 0 then 0 else newTimestamp - forceLit oldTimestamp

        newNumber = fromMaybe 0 $ Map.lookup (Var "symbolic_block_number") cex.vars
        diffNumber = if newNumber == 0 then 0 else newNumber - forceLit oldNumber

      in Just Tx
        { call = SolCall (method.name, args)
        , src = src
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (diffTimestamp, diffNumber)
        }

    _ -> Nothing


cachedOracle :: IORef ContractCache -> IORef SlotCache -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
cachedOracle contractCacheRef slotCacheRef solvers info q = do
  case q of
    PleaseFetchContract addr _ continue -> do
      cache <- liftIO $ readIORef contractCacheRef
      case Map.lookup addr cache of
          Just (Just contract) -> pure $ continue contract
          _                    -> oracle q
    PleaseFetchSlot addr slot continue -> do
      cache <- liftIO $ readIORef slotCacheRef
      case Map.lookup addr cache >>= Map.lookup slot of
        Just (Just value) -> pure $ continue value
        _                 -> oracle q
    _ -> oracle q

  where oracle = Fetch.oracle solvers info

rpcFetcher :: Functor f =>
  f a -> Maybe W256 -> f (Fetch.BlockNumber, a)

rpcFetcher rpc block = (,) block' <$> rpc
  where block' = maybe Fetch.Latest Fetch.BlockNumber block