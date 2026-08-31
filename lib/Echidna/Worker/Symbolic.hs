{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Echidna.Worker.Symbolic (runSymWorker) where

import Control.Concurrent (takeMVar)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (evalRandT)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Control.Monad.State.Strict (MonadIO, StateT, modify', runStateT)
import Control.Monad.Trans (lift)
import Data.Foldable (foldlM)
import Data.IORef (readIORef)
import Data.List.NonEmpty qualified as NEList
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import System.Random (mkStdGen)
import UnliftIO.STM (atomically, dupTChan)

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (Method(..), SolcContract(..))
import EVM.Types hiding (Env, Frame(state), Gas)

import Echidna.ABI
import Echidna.Exec (execTx)
import Echidna.Orphans.Rand ()
import Echidna.Shrink (isShrinkable, shrinkWorkerTests)
import Echidna.Solidity (chooseContract)
import Echidna.SymExec.Common (extractErrors, extractTxs)
import Echidna.SymExec.Exploration (exploreContract, getRandomTargetMethod, getTargetMethodFromTx)
import Echidna.SymExec.Verification (isSuitableToVerifyMethod, verifyMethod)
import Echidna.Test
import Echidna.Test.State (findFailedTests, setAssertionTestState)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Random (rElem)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test
import Echidna.Types.Worker
import Echidna.Worker (listenerLoop, pushWorkerEvent)
import Echidna.Worker.Sequence (callseq)

runSymWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> Maybe Text -- ^ Specified contract name
  -> m (WorkerStopReason, WorkerState)
runSymWorker callback vm dict workerId name = do
  cfg <- asks (.cfg)
  let nworkers = getNFuzzWorkers cfg.campaignConf -- getNFuzzWorkers, NOT getNWorkers
  eventQueue <- asks (.eventQueue)
  chan <- liftIO $ atomically $ dupTChan eventQueue

  flip runStateT initialState $
    flip evalRandT (mkStdGen effectiveSeed) $ do -- unused but needed for callseq
      if isVerificationMode cfg.solConf.testMode then do
        verifyMethods -- No arguments, everything is in this environment
        pure SymbolicVerificationDone
      else do
        lift callback
        listenerLoop listenerFunc chan nworkers
        pure SymbolicExplorationDone

  where

  effectiveSeed = dict.defSeed + workerId
  initialState =
    initialWorkerState { workerId
                       , genDict = dict { defSeed = effectiveSeed }
                       }

  -- We could pattern match on workerType here to ignore WorkerEvents from SymbolicWorkers,
  -- but it may be useful to symexec on top of symexec results to produce multi-transaction
  -- chains where each transaction results in new coverage.
  listenerFunc (_, WorkerEvent _ _ (NewCoverage {transactions})) = do
    void $ callseq vm transactions False
    symexecTxs False transactions
    shrinkAndRandomlyExplore transactions (10 :: Int)
  listenerFunc _ = pure ()

  shrinkAndRandomlyExplore _ 0 = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{shrinkLimit} <- asks (.cfg.campaignConf)
    when (any (isShrinkable shrinkLimit workerId) tests) $ shrinkLoop shrinkLimit

  shrinkAndRandomlyExplore txs n = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{stopOnFail, shrinkLimit} <- asks (.cfg.campaignConf)
    if stopOnFail && any isConclusiveFailure tests then
      lift callback -- >> pure FastFailed
    else if any (isShrinkable shrinkLimit workerId) tests then do
      shrinkLoop shrinkLimit
      shrinkAndRandomlyExplore txs n
    else do
      symexecTxs False txs
      shrinkAndRandomlyExplore txs (n - 1)

  shrinkLoop 0 = return ()
  shrinkLoop n = do
    lift callback
    shrinkWorkerTests workerId vm
    shrinkLoop (n - 1)

  symexecTxs onlyRandom txs = mapM_ symexecTx =<< txsToTxAndVmsSym onlyRandom txs

  -- | Turn a list of transactions into inputs for symexecTx:
  -- (list of txns we're on top of)
  txsToTxAndVmsSym _ [] = pure [(Nothing, vm, [])]
  txsToTxAndVmsSym False txs = do
    -- Separate the last tx, which should be the one increasing coverage
    let (itxs, ltx) = (init txs, last txs)
    ivm <- foldlM (\vm' tx -> snd <$> execTx vm' tx) vm itxs
    -- Split the sequence randomly and select any next transaction
    i <- if length txs == 1 then pure 0 else rElem $ NEList.fromList [1 .. length txs - 1]
    let rtxs = take i txs
    rvm <- foldlM (\vm' tx -> snd <$> execTx vm' tx) vm rtxs
    cfg <- asks (.cfg)
    let targets = cfg.campaignConf.symExecTargets
    if null targets
    then pure [(Just ltx, ivm, txs), (Nothing, rvm, rtxs)]
    else pure [(Nothing, rvm, rtxs)]

  txsToTxAndVmsSym True txs = do
    -- Split the sequence randomly and select any next transaction
    i <- if length txs == 1 then pure 0 else rElem $ NEList.fromList [1 .. length txs - 1]
    let rtxs = take i txs
    rvm <- foldlM (\vm' tx -> snd <$> execTx vm' tx) vm rtxs
    pure [(Nothing, rvm, rtxs)]

  symexecTx (tx, vm', txsBase) = do
    conf <- asks (.cfg)
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    failedTests <- findFailedTests
    let failedTestSignatures = map getAssertionSignature failedTests
    case tx of
      Nothing -> getRandomTargetMethod contract conf.campaignConf.symExecTargets failedTestSignatures >>= \case
        Nothing -> do
          return ()
        Just method -> exploreAndVerify contract method vm' txsBase
      Just t -> getTargetMethodFromTx t contract failedTestSignatures >>= \case
        Nothing -> do
          return ()
        Just method -> do
          exploreAndVerify contract method vm' txsBase

  exploreAndVerify contract method vm' txsBase = do
    (threadId, symTxsChan) <- exploreContract contract method vm'
    modify' (\ws -> ws { runningThreads = [threadId] })
    lift callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan

    modify' (\ws -> ws { runningThreads = [] })
    lift callback

    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error(s) during symbolic exploration: " <> show e)) errors
    unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial explored path(s) during symbolic exploration: " <> unpack e)) partials

    -- We can't do callseq vm' [symTx] because callseq might post the full call sequence as an event
    newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm (txsBase <> [symTx]) False) txs

    when (not newCoverage && null errors && not (null txs)) (
      pushWorkerEvent $ SymExecError "No errors but symbolic execution found valid txs breaking assertions. Something is wrong.")
    unless newCoverage (pushWorkerEvent $ SymExecLog "Symbolic execution finished with no new coverage.")

  verifyMethods = do
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    let allMethods = contract.abiMap
    conf <- asks (.cfg)
    forM_ allMethods (\method -> do
           isSuitable <- isSuitableToVerifyMethod contract method conf.campaignConf.symExecTargets
           if isSuitable
            then symExecMethod contract method
            else pushWorkerEvent $ SymExecError ("Skipped verification of method " <> unpack method.methodSignature)
          )

  symExecMethod contract method = do
    lift callback
    (threadId, symTxsChan) <- verifyMethod method contract vm

    modify' (\ws -> ws { runningThreads = [threadId] })
    lift callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan
    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    modify' (\ws -> ws { runningThreads = [] })
    lift callback
    -- We can't do callseq vm' [symTx] because callseq might post the full call sequence as an event
    newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm [symTx] False) txs
    let methodSignature = unpack method.methodSignature
    unless newCoverage $ do
      unless (null txs) $ error "No new coverage but symbolic execution found valid txs. Something is wrong."
      when (null errors && null partials) $
        setAssertionTestState Unsolvable methodSignature

    unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error(s) solving constraints produced by method " <> methodSignature <> ": " <> show e)) errors
    unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial explored path(s) during symbolic verification of method " <> methodSignature <> ": " <> unpack e)) partials
    when (not (null partials) || not (null errors)) $
      setAssertionTestState Passed methodSignature

    pushWorkerEvent $ SymExecLog ("Symbolic execution finished verifying contract " <> unpack (fromJust name) <> " using a single symbolic transaction.")
