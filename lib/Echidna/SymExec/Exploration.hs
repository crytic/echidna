{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Echidna.SymExec.Exploration where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT, liftIO)
import Control.Monad.State.Strict (MonadState)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text (unpack, Text)
import Data.List.NonEmpty (fromList)
import EVM.Effects (defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic (..), VeriOpts(..), defaultVeriOpts)
import EVM.Types (VMType(..))
import qualified EVM.Types (VM(..))
import Control.Monad.ST (RealWorld)

import Echidna.Types.Campaign (CampaignConf(..), WorkerState)
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.World (World(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Worker (WorkerEvent(..))
import Echidna.Types.Random (rElem)
import Echidna.SymExec.Common (suitableForSymExec, exploreMethod, rpcFetcher, TxOrError(..), PartialsLogs)
import Echidna.Worker (pushWorkerEvent)

-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
-- Also takes an optional Tx argument; this is used as the transaction
--   to symbolize. If none is provided, we do full
--   symbolic execution.
--   The Tx argument, if present, must have a .call value of type SolCall.

getTargetMethodFromTx :: (MonadIO m, MonadReader Echidna.Types.Config.Env m) => Tx -> SolcContract -> [String] -> m (Maybe Method)
getTargetMethodFromTx (Tx { call = SolCall (methodName, _) }) contract failedProperties = do
  env <- ask
  let allMethods = Map.assocs contract.abiMap
      assertSigs = env.world.assertSigs
      (selector, method) = case filter (\(_, m) -> m.name == methodName) allMethods of
        [] -> error $ "Method " ++ show methodName ++ " not found in contract ABI"
        (x:_) -> x

  if (null assertSigs || selector `elem` assertSigs) && suitableForSymExec method && (unpack method.methodSignature) `notElem` failedProperties
  then return $ Just method
  else return Nothing

getTargetMethodFromTx _ _ _ = return Nothing

-- This function selects a random method from the contract's ABI to explore.
-- It uses the campaign configuration to determine which methods are suitable for symbolic execution.
-- Additionally, it filter methods that are associated with failed properties, if any.
getRandomTargetMethod :: (MonadIO m, MonadReader Echidna.Types.Config.Env m) => SolcContract -> [Text] -> [String] -> m (Maybe Method)
getRandomTargetMethod contract targets failedProperties = do
  env <- ask
  let allMethods = Map.assocs contract.abiMap
      assertSigs = env.world.assertSigs
      filterFunc (selector, method) = (null assertSigs || selector `elem` assertSigs) && suitableForSymExec method && (unpack method.methodSignature) `notElem` failedProperties
      filteredMethods = filter filterFunc allMethods

  if null targets
  then case filteredMethods of
    [] -> return Nothing
    _  -> liftIO $ rElem (fromList $ map (Just . snd) filteredMethods)
  else
    let
      targetMethods = filter (\(_, m) -> m.name `elem` targets) allMethods
    in case targetMethods of
      [] -> return Nothing
      _  -> liftIO $ rElem (fromList $ map (Just . snd) targetMethods)

exploreContract :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m) => SolcContract -> Method -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
exploreContract contract method vm = do
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
  let runtimeEnv = defaultEnv { config = defaultConfig { maxWidth = 5, maxDepth = maxExplore, maxBufSize = 12, promiseNoReent = True, debug = isNonInteractive, dumpQueries = False, numCexFuzz = 100 } }
  pushWorkerEvent $ SymExecLog ("Exploring " <> (show method.name))
  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) 1 timeoutSMT $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      -- For now, we will be exploring a single method at a time.
      -- In some cases, this methods list will have only one method, but in other cases, it will have several methods.
      -- This is to improve the user experience, as it will produce results more often, instead having to wait for exploring several
      res <- exploreMethod method contract vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef
      liftIO $ putMVar resultChan res
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)