{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Property where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT, liftIO)
import Control.Monad.State.Strict (MonadState, gets)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text

import EVM.Dapp (DappInfo(..))
import EVM.Effects (defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Fetch (RpcInfo(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (defMemLimit, withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic(..), VeriOpts(..), Postcondition)
import EVM.Types (Expr(..), Prop(..), VMType(..))
import qualified EVM.Types (VM(..))

import Echidna.SymExec.Common
  ( rpcFetcher, checkAssertions, suitableForSymExec
  , exploreMethodTwoPhase
  , TxOrError(..), PartialsLogs
  )
import Echidna.Test (isFoundryMode)
import Echidna.Types.Campaign (CampaignConf(..), WorkerState(..))
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Worker (WorkerType(..), WorkerEvent(..), CampaignEvent(..))
import Echidna.Worker (pushWorkerEvent, pushCampaignEvent)

-- | Postcondition for property mode phase 2: check if property function
-- returned false (i.e., return value is 0).
checkPropertyReturn :: Postcondition
checkPropertyReturn _ = \case
  Success _ _ returnBuf _ ->
    PNeg (PEq (ReadWord (Lit 0) returnBuf) (Lit 0))
  _ -> PBool True

-- | Check if a method is suitable as a phase 1 target in property mode.
isSuitableForPropertyMode :: Method -> Text.Text -> [Text.Text] -> Bool
isSuitableForPropertyMode method prefix symExecTargets =
  if null symExecTargets
  then not (Text.isPrefixOf prefix method.name) && suitableForSymExec method
  else method.name `elem` symExecTargets

-- | Check if a method is a no-argument assertion function suitable for
-- two-phase verification.
isNoArgAssertionTarget :: Method -> Bool
isNoArgAssertionTarget m = null m.inputs
  && not (Text.isInfixOf "_no_symexec" m.name)

-- | Spawn a thread for two-phase property mode verification.
verifyMethodForProperty :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m)
  => Method -> SolcContract -> EVM.Types.VM Concrete -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
verifyMethodForProperty method contract vm = do
  env <- ask
  wid <- gets (.workerId)
  let conf = env.cfg
  dappInfo <- asks (.dapp)
  let
    propertyMethods = filter (\m -> Text.isPrefixOf conf.solConf.prefix m.name) $ Map.elems contract.abiMap
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = RpcInfo (rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock))
    defaultSender = fromJust $ Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = StackBased }
  let hevmConfig = defaultConfig { maxWidth = 5, maxDepth = maxExplore, dumpExprs = True, maxBufSize = 12, promiseNoReent = False, onlyDeployed = True, debug = isNonInteractive }
  let veriOpts = VeriOpts { iterConf = iterConfig, rpcInfo = rpcInfo }
  let runtimeEnv = defaultEnv { config = hevmConfig }
  session <- asks (.fetchSession)
  let methodSig = Text.unpack method.methodSignature
      logTarget target = liftIO $ pushCampaignEvent env (WorkerEvent wid SymbolicWorker (SymExecLog ("Verifying property: " <> methodSig <> " -> " <> Text.unpack target.methodSignature)))

  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) timeoutSMT defMemLimit $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      (res, partials) <- exploreMethodTwoPhase checkPropertyReturn logTarget method propertyMethods contract dappInfo.sources vm defaultSender conf veriOpts solvers rpcInfo session
      liftIO $ putMVar resultChan (res, partials)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)

-- | Spawn a thread for two-phase assertion mode verification (no-arg functions).
verifyMethodForAssertion :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m)
  => [Method] -> Method -> SolcContract -> EVM.Types.VM Concrete -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
verifyMethodForAssertion assertionMethods method contract vm = do
  env <- ask
  wid <- gets (.workerId)
  let conf = env.cfg
  dappInfo <- asks (.dapp)
  let
    isFoundry = isFoundryMode conf.solConf.testMode
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = RpcInfo (rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock))
    defaultSender = fromJust $ Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = StackBased }
  let hevmConfig = defaultConfig { maxWidth = 5, maxDepth = maxExplore, dumpExprs = True, maxBufSize = 12, promiseNoReent = False, onlyDeployed = True, debug = isNonInteractive }
  let veriOpts = VeriOpts { iterConf = iterConfig, rpcInfo = rpcInfo }
  let runtimeEnv = defaultEnv { config = hevmConfig }
  session <- asks (.fetchSession)
  let methodSig' = Text.unpack method.methodSignature
      logTarget target = liftIO $ pushCampaignEvent env (WorkerEvent wid SymbolicWorker (SymExecLog ("Verifying assertion: " <> methodSig' <> " -> " <> Text.unpack target.methodSignature)))

  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) timeoutSMT defMemLimit $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      (res, partials) <- exploreMethodTwoPhase (checkAssertions [0x1] isFoundry) logTarget method assertionMethods contract dappInfo.sources vm defaultSender conf veriOpts solvers rpcInfo session
      liftIO $ putMVar resultChan (res, partials)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)
