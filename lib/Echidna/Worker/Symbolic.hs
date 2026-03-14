{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Echidna.Worker.Symbolic (runSymWorker) where

import Control.Concurrent (takeMVar)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (evalRandT)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Control.Monad.State.Strict (MonadIO, StateT, gets, modify', runStateT)
import Control.Monad.Trans (lift)
import Data.Foldable (foldlM)
import Data.IORef (readIORef)
import Data.List.NonEmpty qualified as NEList
import Data.Map qualified as Map
import Data.Text (Text, pack, unpack)
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
import Echidna.SymExec.Common (extractErrors, extractTxs, suitableForSymExec)
import Echidna.SymExec.Exploration (exploreContract, exploreContractTwoPhase, exploreContractTwoPhaseProperty, getRandomTargetMethod, getTargetMethodFromTx)
import Echidna.SymExec.Property (verifyMethodForProperty, verifyMethodForAssertion, isSuitableForPropertyMode, isNoArgAssertionTarget)
import Echidna.SymExec.Verification (isSuitableToVerifyMethod, verifyMethod)
import Echidna.Test
import Echidna.Test.State (findFailedTests, setAssertionTestState, updateTests)
import Echidna.Transaction (getResultFromVM)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Random (rElem)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test
import Echidna.Types.Test qualified as Test
import Echidna.Types.Tx (TxCall(..), Tx(..), basicTx, maxGasPerBlock)
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
  -> Maybe Text -- ^ Specified contract name
  -> m (WorkerStopReason, WorkerState)
runSymWorker callback onReady vm dict workerId name = do
  cfg <- asks (.cfg)
  let nworkers = getNFuzzWorkers cfg.campaignConf -- getNFuzzWorkers, NOT getNWorkers
  eventQueue <- asks (.eventQueue)
  chan <- liftIO $ atomically $ dupTChan eventQueue
  onReady

  flip runStateT initialState $
    flip evalRandT (mkStdGen effectiveSeed) $ do -- unused but needed for callseq
      if isVerificationMode cfg.solConf.testMode || nworkers == 0 then do
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

  txsBaseLabel txs = case txs of
    [] -> "initial state"
    _  -> show (length txs) <> "-tx sequence ending with " <> showTxCall (last txs)
    where showTxCall t = case t.call of
            SolCall (n, _) -> unpack n
            _ -> "unknown"

  symexecTx (tx, vm', txsBase) = do
    conf <- asks (.cfg)
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    failedTests <- findFailedTests
    let failedTestSignatures = map getAssertionSignature failedTests
    -- Single-phase exploration: only methods matching assertSigs filter
    case tx of
      Nothing -> getRandomTargetMethod contract conf.campaignConf.symExecTargets failedTestSignatures >>= \case
        Nothing -> pure ()
        Just method -> exploreAndVerify contract method vm' txsBase
      Just t -> getTargetMethodFromTx t contract failedTestSignatures >>= \case
        Nothing -> pure ()
        Just method -> exploreAndVerify contract method vm' txsBase
    -- Two-phase exploration: any state-changing method → no-arg targets
    -- Filter to only targets that have registered open tests
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    let stateChanging = filter suitableForSymExec $ Map.elems contract.abiMap
        noArgTargets
          | isPropertyMode conf.solConf.testMode =
              -- Property mode: only echidna_ functions that have open property tests
              let propNames = [n | t <- tests, isOpen t, isPropertyTest t, PropertyTest n _ <- [t.testType]]
              in filter (\m -> null m.inputs && m.name `elem` propNames) $ Map.elems contract.abiMap
          | otherwise =
              -- Assertion mode: only no-arg functions that have open assertion tests
              let assertSigs = [getAssertionSignature t | t <- tests, isOpen t, isAssertionTest t]
              in filter (\m -> isNoArgAssertionTarget m && unpack m.methodSignature `elem` assertSigs) $ Map.elems contract.abiMap
    unless (null noArgTargets || null stateChanging) $ do
      method <- liftIO $ rElem (NEList.fromList stateChanging)
      let baseLabel = txsBaseLabel txsBase
      if isPropertyMode conf.solConf.testMode
        then exploreAndVerifyTwoPhaseProperty contract method noArgTargets vm' txsBase baseLabel
        else exploreAndVerifyTwoPhase contract method noArgTargets vm' txsBase baseLabel

  exploreAndVerify contract method vm' txsBase = do
    -- Single-phase exploration (existing)
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

  exploreAndVerifyTwoPhase contract method targets vm' txsBase baseLabel = do
    conf <- asks (.cfg)
    let dst = conf.solConf.contractAddr
    (threadId2, symTxsChan2) <- exploreContractTwoPhase contract method targets vm' baseLabel
    modify' (\ws -> ws { runningThreads = [threadId2] })
    lift callback

    (symTxs2, partials2) <- liftIO $ takeMVar symTxsChan2
    let txs2 = extractTxs symTxs2
    let errors2 = extractErrors symTxs2

    modify' (\ws -> ws { runningThreads = [] })
    lift callback

    -- For each concrete tx, execute it then check assertion functions
    forM_ txs2 $ \symTx -> do
      (_, vmAfter) <- execTx vm' symTx
      case vmAfter.result of
        Just (VMSuccess _) ->
          updateTests $ \test -> do
            if isOpen test && isAssertionTest test then do
              let fnName = pack (getAssertionFunctionName test)
                  assertTx = basicTx fnName [] symTx.src dst maxGasPerBlock (0, 0)
              (_, vmCheck) <- execTx vmAfter assertTx
              (testValue, vmCheck') <- checkETest test vmCheck
              case testValue of
                BoolValue False -> do
                  wid <- Just <$> gets (.workerId)
                  let test' = test { Test.state = Large 0
                                   , reproducer = txsBase <> [symTx, assertTx]
                                   , vm = Just vmAfter
                                   , result = getResultFromVM vmCheck'
                                   , Test.workerId = wid
                                   }
                  pushWorkerEvent (TestFalsified test')
                  pure $ Just test'
                _ -> pure Nothing
            else pure Nothing
        _ -> pure ()

    unless (null errors2) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error during two-phase assertion exploration: " <> show e)) errors2
    unless (null partials2) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial path during two-phase assertion exploration: " <> unpack e)) partials2

  -- | Two-phase exploration for property mode: execute a state-changing method
  -- symbolically, then check property functions against the resulting states.
  exploreAndVerifyTwoPhaseProperty contract method targets vm' txsBase baseLabel = do
    (threadId2, symTxsChan2) <- exploreContractTwoPhaseProperty contract method targets vm' baseLabel
    modify' (\ws -> ws { runningThreads = [threadId2] })
    lift callback

    (symTxs2, partials2) <- liftIO $ takeMVar symTxsChan2
    let txs2 = extractTxs symTxs2
    let errors2 = extractErrors symTxs2

    modify' (\ws -> ws { runningThreads = [] })
    lift callback

    -- For each concrete tx, execute it and check property functions
    forM_ txs2 $ \symTx -> do
      (_, vmAfter) <- execTx vm symTx
      case vmAfter.result of
        Just (VMSuccess _) ->
          updateTests $ \test -> do
            if isOpen test && isPropertyTest test then do
              (testValue, vmCheck) <- checkETest test vmAfter
              case testValue of
                BoolValue False -> do
                  wid <- Just <$> gets (.workerId)
                  let test' = test { Test.state = Large 0
                                   , reproducer = txsBase <> [symTx]
                                   , vm = Just vmAfter
                                   , result = getResultFromVM vmCheck
                                   , Test.workerId = wid
                                   }
                  pushWorkerEvent (TestFalsified test')
                  pure $ Just test'
                _ -> pure Nothing
            else pure Nothing
        _ -> pure ()

    unless (null errors2) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error during two-phase property exploration: " <> show e)) errors2
    unless (null partials2) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial path during two-phase property exploration: " <> unpack e)) partials2

  verifyMethods = do
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    let allMethods = contract.abiMap
    conf <- asks (.cfg)
    if isPropertyMode conf.solConf.testMode
      then verifyMethodsProperty contract allMethods
      else verifyMethodsAssertion contract allMethods

  -- | Assertion mode verification: single-phase for methods with args,
  -- two-phase for no-arg assertion functions that can only fail via state.
  verifyMethodsAssertion contract allMethods = do
    conf <- asks (.cfg)
    let methods = Map.elems allMethods
        -- No-arg assertion functions: need two-phase
        noArgAssertions = filter isNoArgAssertionTarget methods
        -- State-changing methods with args: used as phase 1 targets
        stateChangingMethods = filter suitableForSymExec methods

    -- Single-phase for methods with args
    forM_ allMethods $ \method -> do
      isSuitable <- isSuitableToVerifyMethod contract method conf.campaignConf.symExecTargets
      if isSuitable
        then symExecMethod contract method
        else pushWorkerEvent $ SymExecError ("Skipped verification of method " <> unpack method.methodSignature)

    -- Two-phase for no-arg assertion functions
    unless (null noArgAssertions || null stateChangingMethods) $ do
      let targetNames = unwords $ map (unpack . (.methodSignature)) noArgAssertions
      pushWorkerEvent $ SymExecLog ("Two-phase assertion verification for [" <> targetNames <> "]")
      forM_ stateChangingMethods $ \method ->
        symExecMethodAssertion contract method noArgAssertions

  -- | Two-phase assertion mode: execute a state-changing method symbolically,
  -- then check no-arg assertion functions against the resulting states.
  symExecMethodAssertion contract method assertionTargets = do
    lift callback
    (threadId, symTxsChan) <- verifyMethodForAssertion assertionTargets method contract vm

    modify' (\ws -> ws { runningThreads = [threadId] })
    lift callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan
    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    modify' (\ws -> ws { runningThreads = [] })
    lift callback

    let methodSignature = unpack method.methodSignature

    pushWorkerEvent $ SymExecLog ("Assertion two-phase " <> methodSignature <> ": " <> show (length txs) <> " tx(es)")

    conf <- asks (.cfg)
    let dst = conf.solConf.contractAddr

    -- For each concrete tx, execute it then explicitly call each assertion function
    forM_ txs $ \symTx -> do
      (_, vm') <- execTx vm symTx
      case vm'.result of
        Just (VMSuccess _) -> do
          -- Re-execute each assertion function on the post-tx state,
          -- then use checkETest which already handles all assertion patterns
          updateTests $ \test -> do
            if isOpen test && isAssertionTest test then do
              let fnName = pack (getAssertionFunctionName test)
                  assertTx = basicTx fnName [] symTx.src dst maxGasPerBlock (0, 0)
              (_, vm'') <- execTx vm' assertTx
              (testValue, vm''') <- checkETest test vm''
              case testValue of
                BoolValue False -> do
                  wid <- Just <$> gets (.workerId)
                  let test' = test { Test.state = Large 0
                                   , reproducer = [symTx, assertTx]
                                   , vm = Just vm'
                                   , result = getResultFromVM vm'''
                                   , Test.workerId = wid
                                   }
                  pushWorkerEvent (TestFalsified test')
                  pure $ Just test'
                _ -> pure Nothing
            else pure Nothing
        _ -> pure ()

    unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error(s) during assertion two-phase for method " <> methodSignature <> ": " <> show e)) errors
    unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial explored path(s) during assertion two-phase for method " <> methodSignature <> ": " <> unpack e)) partials

  -- | Property mode verification: symbolically execute each state-changing
  -- method to find concrete inputs, then check property functions against
  -- the resulting VM states.
  verifyMethodsProperty contract allMethods = do
    conf <- asks (.cfg)
    let prefix = conf.solConf.prefix
        targets = conf.campaignConf.symExecTargets
        methods = Map.elems allMethods
        stateChangingMethods = filter (\m -> isSuitableForPropertyMode m prefix targets) methods

    when (null stateChangingMethods) $
      pushWorkerEvent $ SymExecError "No suitable state-changing methods found for property verification"

    forM_ stateChangingMethods $ \method ->
      symExecMethodProperty contract method

    pushWorkerEvent $ SymExecLog "Property verification finished"

  -- | Symbolically execute a method in property mode: find concrete inputs
  -- for all reachable paths, then check each against all property tests.
  symExecMethodProperty contract method = do
    lift callback
    (threadId, symTxsChan) <- verifyMethodForProperty method contract vm

    modify' (\ws -> ws { runningThreads = [threadId] })
    lift callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan
    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    modify' (\ws -> ws { runningThreads = [] })
    lift callback

    let methodSignature = unpack method.methodSignature

    pushWorkerEvent $ SymExecLog ("Property two-phase " <> methodSignature <> ": " <> show (length txs) <> " tx(es)")

    -- For each concrete tx from symbolic execution, execute it and check properties
    forM_ txs $ \symTx -> do
      (_, vm') <- execTx vm symTx
      case vm'.result of
        Just (VMSuccess _) -> do
          -- Check all open property tests against the post-transaction state
          updateTests $ \test -> do
            if isOpen test && isPropertyTest test then do
              (testValue, vm'') <- checkETest test vm'
              case testValue of
                BoolValue False -> do
                  wid <- Just <$> gets (.workerId)
                  let test' = test { Test.state = Large 0
                                   , reproducer = [symTx]
                                   , vm = Just vm'
                                   , result = getResultFromVM vm''
                                   , Test.workerId = wid
                                   }
                  pushWorkerEvent (TestFalsified test')
                  pure $ Just test'
                _ -> pure Nothing
            else pure Nothing
        _ -> pure ()

    unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error during property verification of " <> methodSignature <> ": " <> show e)) errors
    unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial path during property verification of " <> methodSignature <> ": " <> unpack e)) partials

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

    pushWorkerEvent $ SymExecLog "Assertion verification finished"
