{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Echidna.SymExec (createSymTx) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List (singleton)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, isJust, fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (toList, fromList)
import Optics.Core ((.~), (%))
import Echidna.Solidity (chooseContract)
import Echidna.Types (fromEVM)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import EVM.ABI (AbiValue(..), AbiType(..), Sig(..), decodeAbiValue)
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.SMT (SMTCex(..), SMT2, assertProps)
import EVM (loadContract, resetState)
import EVM.Effects (defaultEnv, defaultConfig)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat), SolverGroup, checkSat)
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, LoopHeuristic (Naive), flattenExpr, extractProps)
import EVM.Types (Addr, VM(..), Frame(..), FrameState(..), VMType(..), Env(..), Expr(..), EType(..), Query(..), Prop(..), BranchCondition(..), W256, word256Bytes, word)
import EVM.Traversals (mapExpr)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState, runStateT)
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)


-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
-- Also takes an optional Tx argument; this is used as the transaction
--   to follow during concolic execution. If none is provided, we do full
--   symbolic execution.
--   The Tx argument, if present, must have a .call value of type SolCall.
createSymTx :: EConfig -> Maybe Text -> [SolcContract] -> Maybe Tx -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
createSymTx cfg name cs tx vm = do
  mainContract <- chooseContract cs name
  exploreContract cfg mainContract tx vm

exploreContract :: EConfig -> SolcContract -> Maybe Tx -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
exploreContract conf contract tx vm = do
  let
    isConc = isJust tx
    allMethods = Map.elems contract.abiMap
    filterMethod name method = method.name == name &&
      case conf.campaignConf.symExecTargets of
        Just ms -> name `elem` ms
        _       -> True
    concMethods (Tx { call = SolCall (methodName, _) }) = filter (filterMethod methodName) allMethods
    concMethods _ = error "`exploreContract` should only be called with Nothing or Just Tx{call=SolCall _} for its tx argument"
    methods = maybe allMethods concMethods tx
    timeout = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = if isConc then Nothing else Just conf.campaignConf.symExecMaxIters
    askSmtIters = if isConc then 0 else conf.campaignConf.symExecAskSMTIters
    rpcInfo = Nothing
    defaultSender = fromJust $ fmap (.dst) tx <|> Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- newEmptyMVar
  doneChan <- newEmptyMVar
  resultChan <- newEmptyMVar

  flip runReaderT defaultEnv $ withSolvers Z3 (fromIntegral conf.campaignConf.symExecNSolvers) timeout $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT defaultEnv $ do
      res <- forM methods $ \method -> do
        let
          fetcher = concOrSymFetcher tx solvers rpcInfo
          dst = conf.solConf.contractAddr
          calldata@(cd, constraints) = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
          vmSym = abstractVM calldata contract.runtimeCode Nothing False
        vmSym' <- liftIO $ stToIO vmSym
        vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
        let vm' = vmReset & execState (loadContract (LitAddr dst))
                          & vmMakeSymbolic
                          & #constraints .~ constraints
                          & #state % #callvalue .~ TxValue
                          & #state % #caller .~ SymAddr "caller"
                          & #state % #calldata .~ cd
                          & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
        -- TODO we might want to switch vm's state.baseState value to to AbstractBase eventually.
        -- Doing so might mess up concolic execution.
        exprInter <- interpret fetcher maxIters askSmtIters Naive vm' runExpr
        models <- liftIO $ mapConcurrently (checkSat solvers) $ manipulateExprInter isConc exprInter
        pure $ mapMaybe (modelToTx dst method conf.solConf.sender defaultSender) models
      liftIO $ putMVar resultChan $ concat res
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- takeMVar threadIdChan
  pure (threadId, resultChan)

-- | Turn the expression returned by `interpret` into into SMT2 values to feed into the solver
manipulateExprInter :: Bool -> Expr End -> [SMT2]
manipulateExprInter isConc = map (assertProps defaultConfig) . middleStep . map (extractProps . simplify) . flattenExpr . simplify where
  middleStep = if isConc then middleStepConc else id
  middleStepConc = map singleton . concatMap (go (PBool True))
  go :: Prop -> [Prop] -> [Prop]
  go _ [] = []
  go acc (h:t) = (PNeg h `PAnd` acc) : go (h `PAnd` acc) t

-- | Sets result to Nothing, and sets gas to ()
vmMakeSymbolic :: VM Concrete s -> VM Symbolic s
vmMakeSymbolic vm
  = VM
  { result         = Nothing
  , state          = frameStateMakeSymbolic vm.state
  , frames         = map frameMakeSymbolic vm.frames
  , env            = vm.env
  , block          = vm.block
  , tx             = vm.tx
  , logs           = vm.logs
  , traces         = vm.traces
  , cache          = vm.cache
  , burned         = ()
  , iterations     = vm.iterations
  , constraints    = vm.constraints
  , config         = vm.config
  , forks          = vm.forks
  , currentFork    = vm.currentFork
  , labels         = vm.labels
  }

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
  }

frameMakeSymbolic :: Frame Concrete s -> Frame Symbolic s
frameMakeSymbolic fr = Frame { context = fr.context, state = frameStateMakeSymbolic fr.state }

modelToTx :: Addr -> Method -> Set Addr -> Addr -> CheckSatResult -> Maybe Tx
modelToTx dst method senders fallbackSender result =
  case result of
    Sat cex ->
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
              _ -> error "Unexpected ABI type in `modelToTx`"

        grabNormalArg argType name
          = case Map.lookup (Var name) cex.vars of
              Just w ->
                decodeAbiValue argType (BS.fromStrict (word256Bytes w))
              Nothing -> -- put a placeholder
                decodeAbiValue argType (BS.repeat 0)

        grabAddressArg name = AbiAddress $ fromMaybe 0 $ Map.lookup (SymAddr name) cex.addrs

        grabArrayArg nElem memberType name = AbiArray nElem memberType $ fromList [grabArg memberType $ name <> T.pack (show n) | n <- [0..nElem] :: [Int]]

        src_ = fromMaybe 0 $ Map.lookup (SymAddr "sender") cex.addrs
        src = if Set.member src_ senders then src_ else fallbackSender

        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

      in Just Tx
        { call = SolCall (method.name, args)
        , src = src
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (0, 0)
        }

    _ -> Nothing

-- | Symbolic variable -> concrete value mapping used during concolic execution.
-- The third member in the tuple is the transaction value.
type Substs = ([(Text, W256)], [(Text, Addr)], W256)

-- | Mirrors hevm's `symAbiArg` function; whenever that changes, we need to change this too
genSubsts :: Tx -> Substs
genSubsts (Tx { call = SolCall (_, abiVals), src, value }) = addOnFinalValues $ fold $ zipWith genVal abiVals (T.pack . ("arg" <>) . show <$> ([1..] :: [Int])) where
  addOnFinalValues (a, b) = (a, ("sender", src):b, value)
  genVal (AbiUInt _ i) name = ([(name, fromIntegral i)], [])
  genVal (AbiInt _ i) name = ([(name, fromIntegral i)], [])
  genVal (AbiBool b) name = ([(name, if b then 1 else 0)], [])
  genVal (AbiAddress addr) name = ([], [(name, addr)])
  genVal (AbiBytes n b) name | n > 0 && n <= 32 = ([(name, word b)], [])
  genVal (AbiArray _ _ vals) name = fold $ zipWith genVal (toList vals) [name <> T.pack (show n) | n <- [0..] :: [Int]]
  genVal _ _ = error "`genSubsts` is not implemented for all API types, mirroring hevm's `symAbiArg` function"
genSubsts _ = error "`genSubsts` should only be called with a `SolCall` transaction argument"

-- | Apply substitutions into an expression
substExpr :: Substs -> Expr a -> Expr a
substExpr (sw, sa, val) = mapExpr go where
  go v@(Var t) = maybe v Lit (lookup t sw)
  go v@(SymAddr t) = maybe v LitAddr (lookup t sa)
  go TxValue = Lit val
  go e = e

-- | Fetcher used during concolic execution.
-- This is the most important function for concolic execution;
-- it determines what branch `interpret` should take.
-- We ensure that this fetcher is always used by setting askSMTIter to 0.
-- We determine what branch to take by substituting concrete values into
-- the provided `Prop`, and then simplifying.
-- We fall back on `Fetch.oracle`.
concFetcher :: Substs -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
concFetcher substs s r (PleaseAskSMT branchcondition pathconditions continue) =
  case simplify (substExpr substs branchcondition) of
    Lit n -> pure (continue (Case (n/=0)))
    simplifiedExpr -> Fetch.oracle s r (PleaseAskSMT simplifiedExpr pathconditions continue)
concFetcher _ s r q = Fetch.oracle s r q

-- | Depending on whether we're doing concolic or full symbolic execution,
-- choose a fetcher to be used in `interpret` (either `concFetcher` or `Fetch.oracle`).
concOrSymFetcher :: Maybe Tx -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
concOrSymFetcher (Just c) = concFetcher $ genSubsts c
concOrSymFetcher Nothing = Fetch.oracle
