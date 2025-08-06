module Echidna.SymExec.Verification where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT, liftIO)
import Control.Monad.State.Strict (MonadState)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import EVM.Effects (defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic (..), VeriOpts(..), defaultVeriOpts, checkAssertions)
import EVM.Types (VMType(..))
import qualified EVM.Types (VM(..))
import Control.Monad.ST (RealWorld)

import Echidna.Types.Campaign (CampaignConf(..), WorkerState)
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Worker (WorkerEvent(..))
import Echidna.SymExec.Common (rpcFetcher, exploreMethod, TxOrError(..), PartialsLogs)
import Echidna.Worker (pushWorkerEvent)

verifyMethod :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m) => Method -> SolcContract -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
verifyMethod method contract vm = do
  conf <- asks (.cfg)
  contractCacheRef <- asks (.fetchContractCache)
  slotCacheRef <- asks (.fetchSlotCache)  
  let
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock)
    defaultSender = fromJust $ Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = Naive}
  let veriOpts = defaultVeriOpts {iterConf = iterConfig, rpcInfo = rpcInfo}
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let runtimeEnv = defaultEnv { config = defaultConfig { maxWidth = 5, maxDepth = maxExplore, maxBufSize = 12, promiseNoReent = False, debug = isNonInteractive, numCexFuzz = 100 } }
  pushWorkerEvent $ SymExecLog ("Verifying " <> (show method.name))
  
  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) 1 timeoutSMT $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      (res, partials) <- exploreMethod method contract vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef (checkAssertions [0x1])
      liftIO $ putMVar resultChan (res, partials)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)