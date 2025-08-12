module Echidna.SymExec.Common where

import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Data.ByteString.Lazy qualified as BS
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector (toList, fromList)
import GHC.IORef (IORef, readIORef)
import Optics.Core ((.~), (%), (%~))
import EVM.ABI (abiKind, AbiKind(Dynamic), AbiValue(..), AbiType(..), Sig(..), decodeAbiValue)
import EVM.Fetch qualified as Fetch
import EVM (loadContract, resetState, forceLit)
import EVM.Effects (TTY, ReadConfig)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (abstractVM, mkCalldata, verifyInputs, VeriOpts(..), checkAssertions)
import EVM.Types (Addr, Frame(..), FrameState(..), VMType(..), EType(..), Expr(..), word256Bytes, Block(..), W256, SMTCex(..), ProofResult(..), Prop(..), Query(..))
import qualified EVM.Types (VM(..), Env(..))
import EVM.Format (formatPartial)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState, runStateT)

import Echidna.Types (fromEVM)
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), maxGasPerBlock)
import Echidna.Types.Cache (ContractCache, SlotCache)

type PartialsLogs = [T.Text]

data TxOrError = TxFromResult Tx | SMTErrorFromResult String
  deriving (Show)

extractTxs :: [TxOrError] -> [Tx]
extractTxs = mapMaybe (\case
  TxFromResult tx -> Just tx
  _ -> Nothing)

extractErrors :: [TxOrError] -> [String]
extractErrors = mapMaybe (\case
  SMTErrorFromResult err -> Just err
  _ -> Nothing)

suitableForSymExec :: Method -> Bool
suitableForSymExec m = not $ null m.inputs 
  && null (filter (\(_, t) -> abiKind t == Dynamic) m.inputs) 
  && not (T.isInfixOf "_no_symexec" m.name)

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
  , constraints    = addBlockConstraints maxTimestampDiff maxNumberDiff vm.block vm.constraints
  , config         = vm.config
  , forks          = vm.forks
  , currentFork    = vm.currentFork
  , labels         = vm.labels
  , osEnv          = vm.osEnv
  , freshVar       = vm.freshVar
  , exploreDepth   = 0
  , keccakPreImgs  = vm.keccakPreImgs
  }

blockMakeSymbolic :: Block -> Block
blockMakeSymbolic b
  = b {
      timestamp = Var "symbolic_block_timestamp"
    , number = Var "symbolic_block_number"
  }

addBlockConstraints :: W256 -> W256 -> Block -> [Prop] -> [Prop]
addBlockConstraints maxTimestampDiff maxNumberDiff block cs =
  cs ++ [
    PGEq (Var "symbolic_block_timestamp") (block.timestamp), PLEq (Sub (Var "symbolic_block_timestamp") (block.timestamp)) $ Lit maxTimestampDiff,
    PGEq (Var "symbolic_block_number") (block.number), PLEq (Sub (Var "symbolic_block_number") (block.number)) $ Lit maxNumberDiff
  ]

senderConstraints :: Set Addr -> [Prop]
senderConstraints as = [foldr (\a b -> POr b (PEq (SymAddr "caller") (LitAddr a))) (PBool False) $ Set.toList as]

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

-- | Convert a n-bit unsigned integer to a n-bit signed integer.
uintToInt :: W256 -> Integer
uintToInt w = fromIntegral (w - 2 ^ (256 :: Int))

modelToTx :: Addr -> Expr EWord -> Expr EWord -> Method -> Set Addr -> Addr -> ProofResult SMTCex String -> TxOrError
modelToTx dst oldTimestamp oldNumber method senders fallbackSender result =
  case result of
    Cex cex ->
      let
        args = zipWith grabArg (snd <$> method.inputs) ["arg" <> T.pack (show n) | n <- [1..] :: [Int]]

        grabArg t
          = case t of
              AbiUIntType _ -> grabNormalArg t
              AbiIntType _ -> grabNormalArgInt t
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

        grabNormalArgInt argType name
          = case Map.lookup (Var name) cex.vars of
              Just w ->
                case argType of
                  AbiIntType n -> AbiInt n (fromIntegral (uintToInt w))
                  _ -> error "Expected AbiIntType"
              Nothing -> -- put a placeholder
                decodeAbiValue argType (BS.repeat 0)

        grabAddressArg name = AbiAddress $ fromMaybe 0 $ Map.lookup (SymAddr name) cex.addrs

        grabArrayArg nElem memberType name = AbiArray nElem memberType $ fromList [grabArg memberType $ name <> "-a-" <> T.pack (show n) | n <- [0..nElem - 1] :: [Int]]

        grabTupleArg memberTypes name = AbiTuple $ fromList [grabArg t $ name <> "-t-" <> T.pack (show n) | (n, t) <- zip ([0..] :: [Int]) (toList memberTypes)]

        src_ = fromMaybe 0 $ Map.lookup (SymAddr "caller") cex.addrs
        src = if Set.member src_ senders then src_ else fallbackSender
        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

        -- If symbolic values for delay (timestamp, number) are unconstrained, no delay is applied
        newTimestamp = fromMaybe 0 $ Map.lookup (Var "symbolic_block_timestamp") cex.vars
        diffTimestamp = if newTimestamp == 0 then 0 else newTimestamp - forceLit oldTimestamp

        newNumber = fromMaybe 0 $ Map.lookup (Var "symbolic_block_number") cex.vars
        diffNumber = if newNumber == 0 then 0 else newNumber - forceLit oldNumber

      in TxFromResult $ Tx
        { call = SolCall (method.name, args)
        , src = src
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (diffTimestamp, diffNumber)
        }

    Error err -> SMTErrorFromResult err
    Unknown err -> SMTErrorFromResult err
    r -> error ("Unexpected value in `modelToTx`: " ++ show r)


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

removeUnknownResults :: [ProofResult SMTCex String] -> ([ProofResult SMTCex String], [ProofResult SMTCex String])
removeUnknownResults results =
  let
    known = filter (\case Unknown _ -> False ; _ -> True) results
    unknown = filter (\case Unknown _ -> True; _ -> False) results
  in (known, unknown)

getUnknownLogs :: [ProofResult SMTCex String] -> [T.Text]
getUnknownLogs = mapMaybe (\case
  Error err -> Just $ T.pack err
  _ -> Nothing)

exploreMethod :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Method -> SolcContract -> EVM.Types.VM Concrete RealWorld -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> IORef ContractCache -> IORef SlotCache -> m ([TxOrError], PartialsLogs)
  
exploreMethod method contract vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef = do
  --liftIO $ putStrLn ("Exploring: " ++ T.unpack method.methodSignature)
  --pushWorkerEvent undefined
  calldataSym@(cd, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  let
    fetcher = cachedOracle contractCacheRef slotCacheRef solvers rpcInfo
    dst = conf.solConf.contractAddr
    vmSym = abstractVM calldataSym contract.runtimeCode Nothing False
  vmSym' <- liftIO $ stToIO vmSym
  vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
  let vm' = vmReset & execState (loadContract (LitAddr dst))
                    & vmMakeSymbolic conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay
                    & #constraints %~ (++ constraints ++ (senderConstraints conf.solConf.sender))
                    & #state % #callvalue .~ TxValue
                    & #state % #caller .~ SymAddr "caller"
                    & #state % #calldata .~ cd
                    & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
  -- TODO we might want to switch vm's state.baseState value to to AbstractBase eventually.
  -- Doing so might mess up concolic execution.
  (_, models, partials) <- verifyInputs solvers veriOpts fetcher vm' (Just $ checkAssertions [0x1])
  let results = map fst models
  --liftIO $ mapM_ TIO.putStrLn partials
  return (map (modelToTx dst vm.block.timestamp vm.block.number method conf.solConf.sender defaultSender) results, map (formatPartial . fst) partials)
