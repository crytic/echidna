{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Echidna.SymExec.Exploration where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT, liftIO)
import Control.Monad.State.Strict (MonadState, gets)
import Data.List (find)
import Data.List.NonEmpty (fromList)
import Data.Map qualified as Map
import Data.Text (unpack, Text)
import UnliftIO.Async (asyncThreadId, wait, withAsync)

import EVM.Effects (defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Fetch (RpcInfo(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic (..), VeriOpts(..))
import EVM.Types (VMType(..))
import qualified EVM.Types (VM(..))

import Echidna.SymExec.Common (rpcFetcher, symExecAbstractArith, TxOrError(..), PartialsLogs)
import Echidna.SymExec.Invariant (SymbolicSearch, exploreSearchWithProgress, searchDepth, searchName, selectedMethod, selectedTarget)
import Echidna.SymExec.Progress (withProgress)
import Echidna.Test (isFoundrySymbolicName)
import Echidna.Types.Campaign (CampaignConf(..), WorkerState(..))
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.Random (rElem)
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..), WorkerType(SymbolicWorker))
import Echidna.Types.World (World(..))
import Echidna.Worker (pushCampaignEvent, pushWorkerEvent)

getTargetMethodFromTx :: (MonadIO m, MonadReader Echidna.Types.Config.Env m) => Tx -> SolcContract -> [String] -> m (Maybe Method)
getTargetMethodFromTx (Tx { call = SolCall ("", _) }) _ _ = return Nothing
getTargetMethodFromTx (Tx { call = SolCall (methodName, _) }) contract failedProperties = do
  env <- ask
  let allMethods = Map.assocs contract.abiMap
      assertSigs = env.world.assertSigs
      (selector, method) = case filter (\(_, m) -> m.name == methodName) allMethods of
        [] -> error $ "Method " ++ show methodName ++ " not found in contract ABI"
        (x:_) -> x

  if selectedMethod env.cfg contract method && (null assertSigs || selector `elem` assertSigs)
    && (unpack method.methodSignature) `notElem` failedProperties
  then return $ Just method
  else return Nothing

getTargetMethodFromTx _ _ _ = return Nothing

-- This function selects a random method from the contract's ABI to explore.
-- It uses the campaign configuration to determine which methods are suitable for symbolic execution.
-- Additionally, it filter methods that are associated with failed properties, if any.
getRandomTargetMethod :: (MonadIO m, MonadReader Echidna.Types.Config.Env m) => SolcContract -> [Text] -> [String] -> m (Maybe Method)
getRandomTargetMethod contract targets failedProperties = do
  env <- ask
  let allMethods = filter (selectedMethod env.cfg contract . snd) (Map.assocs contract.abiMap)
      assertSigs = env.world.assertSigs
      filterFunc (selector, method) =
        (null assertSigs || selector `elem` assertSigs)
        && unpack method.methodSignature `notElem` failedProperties
      filteredMethods = filter filterFunc allMethods

  if null targets
  then case filteredMethods of
    [] -> return Nothing
    _  -> liftIO $ rElem (fromList $ map (Just . snd) filteredMethods)
  else
    let
      targetMethods = filter (selectedTarget targets . snd) allMethods
    in case targetMethods of
      [] -> return Nothing
      _  -> liftIO $ rElem (fromList $ map (Just . snd) targetMethods)

isSuitableToVerifyMethod :: (MonadIO m, MonadReader Echidna.Types.Config.Env m) => SolcContract -> Method -> [Text] -> m Bool
isSuitableToVerifyMethod contract method symExecTargets = do
  env <- ask
  let allMethods = Map.assocs contract.abiMap
      assertSigs = env.world.assertSigs
      (selector, _) = case find (\(_, m) -> m == method) allMethods of
        Nothing -> error $ "Method " ++ show method.name ++ " not found in contract ABI"
        Just x  -> x
      -- "check" and "prove" functions are explicit symbolic test entry points
      -- (as in Foundry and Halmos), so they are verified even when the static
      -- analysis found no assertion in them.
      isSymbolicEntryPoint = isFoundrySymbolicName method.name

  return $ selectedMethod env.cfg contract method &&
    if null symExecTargets
    then isSymbolicEntryPoint || null assertSigs || selector `elem` assertSigs
    else selectedTarget symExecTargets method


data SearchMode = Explore | Verify deriving Eq

-- | Shared solver setup for corpus exploration and verification.
runSearch :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m, MonadState WorkerState m) => SearchMode -> SymbolicSearch -> EVM.Types.VM Concrete -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
runSearch mode search vm = do
  env <- ask
  workerId <- gets (.workerId)
  let conf = env.cfg
  let
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = RpcInfo (rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock))

  resultChan <- liftIO newEmptyMVar
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let iterConfig = IterConfig { maxIter = maxIters, maxConcreteIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = StackBased}
  let hevmConfig = defaultConfig { maxWidth = 5, maxDepth = maxExplore, maxBufSize = 12, maxDynSize = 128, promiseNoReent = False, onlyDeployed = True, debug = isNonInteractive, dumpExprs = mode == Verify, dumpQueries = False, abstractArith = symExecAbstractArith }
  let veriOpts = VeriOpts {iterConf = iterConfig, rpcInfo = rpcInfo}
  let runtimeEnv = defaultEnv { config = hevmConfig }
  session <- asks (.fetchSession)
  let action = if mode == Verify then "Checking " else "Exploring "
      bound = searchDepth conf search
      budget = maybe "" (\s -> "; campaign limit " <> show s <> "s") conf.uiConf.maxTime
      report = pushCampaignEvent env . WorkerEvent workerId SymbolicWorker . SymExecLog
  pushWorkerEvent $ SymExecLog (action <> unpack (searchName search)
    <> " (up to " <> show bound <> " transactions; SMT timeout "
    <> show conf.campaignConf.symExecTimeout <> "s/query" <> budget <> ")")
  liftIO $ withProgress bound report $ \progress -> flip runReaderT runtimeEnv $
    withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) timeoutSMT conf.campaignConf.symExecSolverMemory $ \solvers ->
    withAsync (exploreSearchWithProgress progress search vm conf veriOpts solvers rpcInfo session) $ \worker -> do
      res <- wait worker
      liftIO $ putMVar resultChan res
      pure (asyncThreadId worker, resultChan)
