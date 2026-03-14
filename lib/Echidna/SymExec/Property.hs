{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Property where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT)
import Control.Monad.State.Strict (MonadState, execState, runStateT)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text

import EVM (initialContract, loadContract, resetState, symbolify)
import EVM.ABI (Sig(..), selector)
import EVM.Dapp (DappInfo(..))
import EVM.Effects (TTY, ReadConfig, defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Fetch qualified as Fetch
import EVM.Fetch (RpcInfo(..))
import EVM.Format (formatPartialDetailed)
import EVM.Solidity (SolcContract(..), SourceCache(..), Method(..))
import EVM.Solvers (SolverGroup, defMemLimit, withSolvers)
import EVM.SymExec (mkCalldata, verifyInputsWithHandler, IterConfig(..), LoopHeuristic(..), VeriOpts(..), Postcondition)
import EVM.Types (Addr, Contract(..), VMType(..), EType(..), Expr(..), Block(..), Prop(..), isQed)
import qualified EVM.Types (VM(..))

import Echidna.SymExec.Common
  ( rpcFetcher, modelToTx, suitableForSymExec
  , blockMakeSymbolic, addBlockConstraints, senderConstraints
  , TxOrError(..), PartialsLogs
  )
import Echidna.Test (isFoundryMode)
import Echidna.Types (fromEVM)
import Echidna.Types.Campaign (CampaignConf(..), WorkerState)
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (TxConf(..))
import Echidna.Types.Worker (WorkerEvent(..))
import Echidna.Worker (pushWorkerEvent)
import Optics.Core ((.~), (%), (%~))

-- | Convert a symbolic contract expression (from a Success leaf) to a
-- concrete Contract value suitable for use in a VM's env.contracts map.
fromEContract :: Expr EContract -> Contract
fromEContract (C code storage tStorage balance nonce) =
  initialContract code
    & #storage .~ storage
    & #tStorage .~ tStorage
    & #balance .~ balance
    & #nonce .~ nonce
fromEContract _ = error "fromEContract: unexpected non-C expression"

-- | Postcondition for property mode phase 2: check if property function
-- returned false (i.e., return value is 0). A return value of 0 means the
-- property was violated.
checkPropertyReturn :: Postcondition
checkPropertyReturn _ = \case
  Success _ _ returnBuf _ ->
    -- Property should return true (non-zero). If it returns 0 (false),
    -- we have a violation. The postcondition says "this should hold",
    -- so we express "return != 0".
    PNeg (PEq (ReadWord (Lit 0) returnBuf) (Lit 0))
  _ -> PBool True  -- reverts/failures are not property violations

-- | Check if a method is suitable for property mode verification.
-- In property mode, we want to symbolically execute state-changing functions
-- (not the property functions themselves). Property functions are identified
-- by the configured prefix (e.g., "echidna_").
isSuitableForPropertyMode :: Method -> Text.Text -> [Text.Text] -> Bool
isSuitableForPropertyMode method prefix symExecTargets =
  if null symExecTargets
  then not (Text.isPrefixOf prefix method.name) && suitableForSymExec method
  else method.name `elem` symExecTargets

-- | Two-phase symbolic execution for property mode.
--
-- Phase 1: Symbolically execute a state-changing method with a trivially-true
-- postcondition (no solver calls). This collects all reachable Success leaves,
-- each containing symbolic contract states and path constraints.
--
-- Phase 2: For each Success leaf and each property method, reconstruct a VM
-- with the symbolic contract states, execute the property function, and check
-- if it can return false. The solver finds concrete values for phase 1's
-- symbolic variables (calldata, caller, etc.) that cause the property to fail.
exploreMethodForProperty :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Method -> [Method] -> SolcContract -> SourceCache -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)

exploreMethodForProperty method propertyMethods _contract _sources vm defaultSender conf veriOpts solvers rpcInfo session = do
  -- Phase 1: Execute state-changing method symbolically
  calldataSym@(_, constraints) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
  let
    cd = fst calldataSym
    fetcher = Fetch.oracle solvers (Just session) rpcInfo
    dst = conf.solConf.contractAddr
  vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
  let
    vm' = vmReset & execState (loadContract (LitAddr dst))
                  & #tx % #isCreate .~ False
                  & #state % #callvalue .~ (if isFoundryMode conf.solConf.testMode then Lit 0 else TxValue)
                  & #state % #caller .~ SymAddr "caller"
                  & #state % #calldata .~ cd

    vm'' = symbolify vm'
        & #block %~ blockMakeSymbolic
        & #constraints %~ (addBlockConstraints conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay vm'.block)
        & #constraints %~ (++ constraints ++ senderConstraints conf.solConf.sender)

  -- Phase 1: PBool True postcondition → no solver calls, all Success paths return Qed
  (phase1Models, phase1Partials) <- verifyInputsWithHandler solvers veriOpts fetcher vm'' (\_ _ -> PBool True) Nothing

  -- Extract Success leaves with their path constraints and symbolic contract states
  let successLeaves = [(props, contracts) | (_, Success props _ _ contracts) <- phase1Models]

  -- Phase 2: For each Success leaf, check each property function
  allResults <- concat <$> mapM (checkLeaf fetcher dst vm cd method conf defaultSender solvers veriOpts propertyMethods) successLeaves

  let allPartialLogs = map (\(p, _) -> formatPartialDetailed Nothing Map.empty p) phase1Partials
  return (allResults, allPartialLogs)

  where

  -- | Phase 2 for a single Success leaf: check all property methods
  checkLeaf fetcher dst vm0 cd method0 conf0 defaultSender0 solvers0 veriOpts0 propMethods (leafConstraints, leafContracts) = do
    let convertedContracts = Map.map fromEContract leafContracts
    concat <$> mapM (checkProperty fetcher dst vm0 cd method0 conf0 defaultSender0 solvers0 veriOpts0 convertedContracts leafConstraints) propMethods

  -- | Phase 2 for a single (leaf, property) pair
  checkProperty fetcher dst vm0 cd method0 conf0 defaultSender0 solvers0 veriOpts0 convertedContracts leafConstraints propMethod = do
    vmReset2 <- liftIO $ snd <$> runStateT (fromEVM resetState) vm0
    let
      propCd = ConcreteBuf (selector propMethod.methodSignature)
      -- Replace contracts with phase 1's symbolic state, keeping originals as fallback
      vm2 = vmReset2 & #env % #contracts %~ Map.union convertedContracts
                     & execState (loadContract (LitAddr dst))
                     & #tx % #isCreate .~ False
                     & #state % #callvalue .~ Lit 0
                     & #state % #caller .~ SymAddr "caller"
                     & #state % #calldata .~ propCd
      -- Carry over phase 1's path constraints so the solver must satisfy both phases
      vm2' = symbolify vm2
          & #constraints .~ leafConstraints

    (phase2Models, _) <- verifyInputsWithHandler solvers0 veriOpts0 fetcher vm2' checkPropertyReturn Nothing
    let results2 = filter (\(r, _) -> not (isQed r)) phase2Models & map fst
    return $ map (modelToTx dst vm0.block.timestamp vm0.block.number method0 conf0.solConf.sender defaultSender0 cd) results2

-- | Two-phase symbolic verification for property mode.
-- Phase 1: Symbolically execute the state-changing method to collect all
-- reachable symbolic states (no solver calls).
-- Phase 2: For each symbolic state, execute each property function and use
-- the solver to find inputs that make the property return false.
verifyMethodForProperty :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m) => Method -> SolcContract -> EVM.Types.VM Concrete -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
verifyMethodForProperty method contract vm = do
  conf <- asks (.cfg)
  dappInfo <- asks (.dapp)
  let
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = RpcInfo (rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock))
    defaultSender = fromJust $ Set.lookupMin conf.solConf.sender <|> Just 0
    -- Find property methods: functions with the configured prefix (e.g., "echidna_")
    prefix = conf.solConf.prefix
    propertyMethods = filter (\m -> Text.isPrefixOf prefix m.name) $ Map.elems contract.abiMap

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = StackBased}
  let hevmConfig = defaultConfig { maxWidth = 5, maxDepth = maxExplore, dumpExprs = True, maxBufSize = 12, promiseNoReent = False, onlyDeployed = True, debug = isNonInteractive }
  let veriOpts = VeriOpts {iterConf = iterConfig, rpcInfo = rpcInfo}
  let runtimeEnv = defaultEnv { config = hevmConfig }
  session <- asks (.fetchSession)
  pushWorkerEvent $ SymExecLog ("Verifying (property mode) " <> (show method.name))

  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) timeoutSMT defMemLimit $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      (res, partials) <- exploreMethodForProperty method propertyMethods contract dappInfo.sources vm defaultSender conf veriOpts solvers rpcInfo session
      liftIO $ putMVar resultChan (res, partials)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)
