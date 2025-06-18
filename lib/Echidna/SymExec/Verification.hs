module Echidna.SymExec.Verification where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, runReaderT, liftIO)
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import EVM.Effects (defaultEnv, defaultConfig, Config(..), config)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic (..), VeriOpts(..))
import EVM.Types (VMType(..))
import qualified EVM.Types (VM(..))
import Control.Monad.ST (RealWorld)

import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), operationMode)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.SymExec.Common (rpcFetcher, exploreMethod, TxOrError(..), PartialsLogs)

verifyMethod :: (MonadIO m, MonadThrow m, MonadReader Env m) => Method -> SolcContract -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
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
  let veriOpts = VeriOpts {iterConf = iterConfig, simp = True, rpcInfo = rpcInfo}
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let runtimeEnv = defaultEnv { config = defaultConfig { maxWidth = 5, maxDepth = maxExplore, maxBufSize = 12, promiseNoReent = False, debug = isNonInteractive, numCexFuzz = 100 } }

  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) 1 timeoutSMT $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      (res, partials) <- exploreMethod method contract vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef
      liftIO $ putMVar resultChan (res, partials)
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)