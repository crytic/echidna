module Echidna.SymExec.Verification where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT, liftIO)
import Control.Monad.State.Strict (MonadState)
import Data.Map (assocs)
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Set qualified as Set
import Data.List (find)
import EVM.Effects (defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers)
import EVM.Dapp (DappInfo(..))
import EVM.SymExec (IterConfig(..), LoopHeuristic (..), VeriOpts(..))
import EVM.Types (VMType(..))
import EVM.Fetch (RpcInfo(..))
import qualified EVM.Types (VM(..))
import Control.Monad.ST (RealWorld)

import Echidna.Types.Campaign (CampaignConf(..), WorkerState)
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.World (World(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Worker (WorkerEvent(..))
import Echidna.SymExec.Common (rpcFetcher, exploreMethod, suitableForSymExec, TxOrError(..), PartialsLogs)
import Echidna.Worker (pushWorkerEvent)

isSuitableToVerifyMethod :: (MonadIO m, MonadReader Echidna.Types.Config.Env m) => SolcContract -> Method -> [Text.Text] -> m Bool
isSuitableToVerifyMethod contract method symExecTargets = do
  env <- ask
  let allMethods = assocs contract.abiMap
      assertSigs = env.world.assertSigs
      (selector, _) = case find (\(_, m) -> m == method) allMethods of
        Nothing -> error $ "Method " ++ show method.name ++ " not found in contract ABI"
        Just x  -> x

  return $
    if null symExecTargets
    then (null assertSigs || selector `elem` assertSigs) && suitableForSymExec method
    else method.name `elem` symExecTargets


verifyMethod :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m) => Method -> SolcContract -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
verifyMethod method contract vm = do
  conf <- asks (.cfg)
  dappInfo <- asks (.dapp)
  contractCacheRef <- asks (.fetchContractCache)
  slotCacheRef <- asks (.fetchSlotCache)
  let
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = RpcInfo (rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock)) Nothing Nothing Nothing
    defaultSender = fromJust $ Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = Naive}
  let hevmConfig = defaultConfig { maxWidth = 5, maxDepth = maxExplore, dumpExprs = True, maxBufSize = 12, promiseNoReent = False, onlyDeployed = True, debug = isNonInteractive, numCexFuzz = 0 }
  let veriOpts = VeriOpts {iterConf = iterConfig, rpcInfo = rpcInfo}
  let runtimeEnv = defaultEnv { config = hevmConfig }
  pushWorkerEvent $ SymExecLog ("Verifying " <> (show method.name))

  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) 1 timeoutSMT $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      (res, partials) <- exploreMethod method contract dappInfo.sources vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef
      liftIO $ putMVar resultChan (res, partials)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)
