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
import Echidna.SymExec.Exploration (SearchMode(..), getRandomTargetMethod, getTargetMethodFromTx, isSuitableToVerifyMethod, runSearch)
import Echidna.SymExec.Invariant (SymbolicSearch(..), invariantSearch, searchMethods, searchName)
import Echidna.Test
import Echidna.Test.State (findFailedTests, setAssertionTestState)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Random (rElem)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker
import Echidna.Worker (listenerLoop, pushWorkerEvent)
import Echidna.Worker.Sequence (callseq)

runSymWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> m () -- ^ Called after subscribing to campaign events
  -> VM Concrete -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> [(FilePath, [Tx])] -- ^ Initial corpus
  -> Maybe Text -- ^ Specified contract name
  -> m (WorkerStopReason, WorkerState)
runSymWorker callback onReady vm dict workerId initialCorpus name = do
  cfg <- asks (.cfg)
  let nworkers = getNFuzzWorkers cfg.campaignConf -- getNFuzzWorkers, NOT getNWorkers
  eventQueue <- asks (.eventQueue)
  chan <- liftIO $ atomically $ dupTChan eventQueue
  onReady

  flip runStateT initialState $
    flip evalRandT (mkStdGen effectiveSeed) $ do -- unused but needed for callseq
      forM_ initialCorpus $ \(_, txs) -> do
        (endVm, _) <- callseq vm txs True
        symexecTx (Nothing, endVm, txs)
      if isVerificationMode cfg.solConf.testMode || nworkers == 0 then do
        verifyMethods -- No arguments, everything is in this environment
        unless (isVerificationMode cfg.solConf.testMode) $ shrinkAndRandomlyExplore [] 0
        pure $ if isVerificationMode cfg.solConf.testMode then SymbolicVerificationDone else SymbolicExplorationDone
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
    shrinkWorkerTests workerId vm

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
    then pure [(Just ltx, ivm, itxs), (Nothing, rvm, rtxs)]
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
    tests <- asks (.testRefs) >>= liftIO . traverse readIORef
    case invariantSearch conf contract tests of
      Just search -> symbolicSearch Explore search vm' txsBase
      Nothing -> do
        failedTests <- findFailedTests
        let failedTestSignatures = map getAssertionSignature (filter isAssertionTest failedTests)
        method <- case tx of
          Nothing -> getRandomTargetMethod contract conf.campaignConf.symExecTargets failedTestSignatures
          Just t -> getTargetMethodFromTx t contract failedTestSignatures
        forM_ method $ \m -> symbolicSearch Explore (SingleMethod m) vm' txsBase

  verifyMethods = do
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    let allMethods = contract.abiMap
    conf <- asks (.cfg)
    tests <- asks (.testRefs) >>= liftIO . traverse readIORef
    let shared = invariantSearch conf contract tests
        searched = maybe [] searchMethods shared
    forM_ shared $ \search -> symbolicSearch Verify search vm []
    unless (isVerificationMode conf.solConf.testMode && conf.campaignConf.symExecSeqLen > 1) $
      forM_ allMethods $ \method -> unless (method `elem` searched) $ do
        suitable <- isSuitableToVerifyMethod contract method conf.campaignConf.symExecTargets
        if suitable
          then symbolicSearch Verify (SingleMethod method) vm []
          else pushWorkerEvent $ SymExecLog ("Skipping " <> unpack method.methodSignature)

  symbolicSearch mode search initialVm prefix = do
    conf <- asks (.cfg)
    lift callback
    (threadId, symTxsChan) <- runSearch mode search initialVm

    modify' (\ws -> ws { runningThreads = [threadId] })
    lift callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan
    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    modify' (\ws -> ws { runningThreads = [] })
    lift callback
    -- Replay from the deployment state so coverage and failures retain the
    -- concrete prefix as well as the symbolic suffix.
    newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm (prefix <> symTx) False) txs
    let label = unpack (searchName search)
        incomplete = not (null errors) || not (null partials)
    when (mode == Verify && isVerificationMode conf.solConf.testMode) $
      case search of
        SingleMethod method -> when (null txs || incomplete) $
          setAssertionTestState (if incomplete then Passed else Unsolvable) (unpack method.methodSignature)
        BoundedVerification methods -> unless incomplete $
          forM_ methods $ \method -> setAssertionTestState Passed (unpack method.methodSignature)
        InvariantSearch {} -> pure ()

    forM_ errors $ \err -> pushWorkerEvent $ SymExecError ("Error solving " <> label <> ": " <> err)
    forM_ partials $ \partial -> pushWorkerEvent $ SymExecError ("Partial path while checking " <> label <> ": " <> unpack partial)
    when (mode == Explore && not newCoverage) $
      pushWorkerEvent $ SymExecLog "Symbolic execution finished with no new coverage."
    let scope = case search of
          BoundedVerification {} -> " (up to " <> show conf.campaignConf.symExecSeqLen <> " transactions)"
          _ -> ""
    pushWorkerEvent $ SymExecLog ("Symbolic execution finished checking " <> label <> scope)
