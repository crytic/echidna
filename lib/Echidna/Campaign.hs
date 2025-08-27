{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Echidna.Campaign where

import Control.Concurrent
import Control.DeepSeq (force)
import Control.Monad (replicateM, when, unless, void, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom, RandT, evalRandT)
import Control.Monad.Reader (MonadReader, asks, liftIO, ask)
import Control.Monad.State.Strict
  (MonadState(..), StateT(..), gets, MonadIO, modify')
import Control.Monad.ST (RealWorld)
import Control.Monad.Trans (lift)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Data.Foldable (foldlM)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEList
import Data.Map qualified as Map
import Data.Map (Map, (\\))
import Data.Maybe (isJust, mapMaybe, fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, unpack)
import Data.Time (LocalTime)
import Data.Vector qualified as V
import System.Random (mkStdGen)

import EVM (cheatCode)
import EVM.ABI (getAbi, AbiType(AbiAddressType, AbiTupleType), AbiValue(AbiAddress, AbiTuple), abiValueType)
import EVM.Dapp (DappInfo(..))
import EVM.Types hiding (Env, Frame(state), Gas)
import EVM.Solidity (SolcContract(..), Method(..))

import Echidna.ABI
import Echidna.Events (extractEventValues)
import Echidna.Exec
import Echidna.Mutator.Corpus
import Echidna.Shrink (shrinkTest)
import Echidna.Solidity (chooseContract)
import Echidna.SymExec.Common (extractTxs, extractErrors)
import Echidna.SymExec.Symbolic (forceAddr)
import Echidna.SymExec.Exploration (exploreContract, getTargetMethodFromTx, getRandomTargetMethod)
import Echidna.SymExec.Verification (verifyMethod, isSuitableToVerifyMethod)
import Echidna.Test
import Echidna.Transaction
import Echidna.Types.Campaign
import Echidna.Types.Corpus (Corpus, corpusSize)
import Echidna.Types.Coverage (coverageStats)
import Echidna.Types.Config
import Echidna.Types.Random (rElem)
import Echidna.Types.Signature (FunctionName)
import Echidna.Types.Test
import Echidna.Types.Test qualified as Test
import Echidna.Types.Tx (TxCall(..), Tx(..))
import Echidna.Types.Worker
import Echidna.Worker

instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccessful :: [EchidnaTest] -> Bool
isSuccessful =
  all (\case { Passed -> True; Open -> True; _ -> False; } . (.state))

-- | Run all the transaction sequences from the corpus and accumulate campaign
-- state. Can be used to minimize corpus as the final campaign state will
-- contain minimized corpus without sequences that didn't increase the coverage.
replayCorpus
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete RealWorld -- ^ VM to start replaying from
  -> [(FilePath, [Tx])] -- ^ corpus to replay
  -> m ()
replayCorpus vm txSeqs =
  forM_ (zip [1..] txSeqs) $ \(i, (file, txSeq)) -> do
    let maybeFaultyTx =
          List.find (\tx -> LitAddr tx.dst `notElem` Map.keys vm.env.contracts) $
            List.filter (\case Tx { call = NoCall } -> False; _ -> True) txSeq
    case maybeFaultyTx of
      Nothing -> do
        _ <- callseq vm txSeq
        pushWorkerEvent (TxSequenceReplayed file i (length txSeqs))
      Just faultyTx ->
        pushWorkerEvent (TxSequenceReplayFailed file faultyTx)

runWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => WorkerType
  -> StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete RealWorld -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> [(FilePath, [Tx])]
  -- ^ Initial corpus of transactions
  -> Int     -- ^ Test limit for this worker
  -> Maybe Text -- ^ Specified contract name
  -> m (WorkerStopReason, WorkerState)
runWorker SymbolicWorker callback vm dict workerId initialCorpus _ name =
  runSymWorker callback vm dict workerId initialCorpus name
runWorker FuzzWorker callback vm dict workerId initialCorpus testLimit _ =
  runFuzzWorker callback vm dict workerId initialCorpus testLimit

runSymWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete RealWorld -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> [(FilePath, [Tx])]
  -- ^ Initial corpus of transactions
  -> Maybe Text -- ^ Specified contract name
  -> m (WorkerStopReason, WorkerState)
runSymWorker callback vm dict workerId _ name = do
  cfg <- asks (.cfg)
  let nworkers = getNFuzzWorkers cfg.campaignConf -- getNFuzzWorkers, NOT getNWorkers
  eventQueue <- asks (.eventQueue)
  chan <- liftIO $ dupChan eventQueue

  flip runStateT initialState $
    flip evalRandT (mkStdGen effectiveSeed) $ do -- unused but needed for callseq
      if (cfg.campaignConf.workers == Just 0) && (cfg.campaignConf.seqLen == 1) then do
        verifyMethods -- No arguments, everything is in this environment
        pure SymbolicVerificationDone
      else do
        lift callback
        listenerLoop listenerFunc chan nworkers
        pure SymbolicExplorationDone

  where

  effectiveSeed = dict.defSeed + workerId
  effectiveGenDict = dict { defSeed = effectiveSeed }
  initialState =
    WorkerState { workerId
                , genDict = effectiveGenDict
                , newCoverage = False
                , ncallseqs = 0
                , ncalls = 0
                , totalGas = 0
                , runningThreads = []
                }

  -- We could pattern match on workerType here to ignore WorkerEvents from SymbolicWorkers,
  -- but it may be useful to symexec on top of symexec results to produce multi-transaction
  -- chains where each transaction results in new coverage.
  listenerFunc (_, WorkerEvent _ _ (NewCoverage {transactions})) = do
    void $ callseq vm transactions
    symexecTxs False transactions
    shrinkAndRandomlyExplore transactions (10 :: Int)
  listenerFunc _ = pure ()

  shrinkAndRandomlyExplore _ 0 = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{shrinkLimit} <- asks (.cfg.campaignConf)
    when (any shrinkable tests) $ shrinkLoop shrinkLimit

  shrinkAndRandomlyExplore txs n = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{stopOnFail, shrinkLimit} <- asks (.cfg.campaignConf)
    if stopOnFail && any final tests then
      lift callback -- >> pure FastFailed
    else if any shrinkable tests then do
      shrinkLoop shrinkLimit
      shrinkAndRandomlyExplore txs n
    else do
      symexecTxs False txs
      shrinkAndRandomlyExplore txs (n - 1)


  shrinkable test =
    case test.state of
      -- we shrink only tests which were solved on this
      -- worker, see 'updateOpenTest'
      Large _ | test.workerId == Just workerId -> True
      _       -> False

  final test =
    case test.state of
      Solved   -> True
      Failed _ -> True
      _        -> False


  shrinkLoop 0 = return ()
  shrinkLoop n = do
    lift callback
    updateTests $ \test -> do
      if test.workerId == Just workerId then
        shrinkTest vm test
      else
        pure Nothing
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
    newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm (txsBase <> [symTx])) txs

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
    let methodSignature = unpack method.methodSignature
    if not (null partials) || not (null errors) then do
      unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error(s) solving constraints produced by method " <> methodSignature <> ": " <> show e)) errors
      unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial explored path(s) during symbolic verification of method " <> methodSignature <> ": " <> unpack e)) partials
      updateTests $ \test -> do
          if isOpen test && isAssertionTest test && getAssertionSignature test == methodSignature then
              pure $ Just $ test { Test.state = Passed }
          else
            pure $ Just test
    else do
      -- We can't do callseq vm' [symTx] because callseq might post the full call sequence as an event
      newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm [symTx]) txs

      unless newCoverage ( do
        unless (null txs) $ error "No new coverage but symbolic execution found valid txs. Something is wrong."
        updateTests $ \test -> do
          if isOpen test && isAssertionTest test && getAssertionSignature test == methodSignature then
                pure $ Just $ test { Test.state = Unsolvable }
          else
            pure $ Just test
        pushWorkerEvent $ SymExecLog ("Symbolic execution finished verifying contract " <> unpack (fromJust name) <> " using a single symbolic transaction."))

-- | Run a fuzzing campaign given an initial universe state, some tests, and an
-- optional dictionary to generate calls with. Return the 'Campaign' state once
-- we can't solve or shrink anything.
runFuzzWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete RealWorld -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> [(FilePath, [Tx])]
  -- ^ Initial corpus of transactions
  -> Int     -- ^ Test limit for this worker
  -> m (WorkerStopReason, WorkerState)
runFuzzWorker callback vm dict workerId initialCorpus testLimit = do
  let
    effectiveSeed = dict.defSeed + workerId
    effectiveGenDict = dict { defSeed = effectiveSeed }
    initialState =
      WorkerState { workerId
                  , genDict = effectiveGenDict
                  , newCoverage = False
                  , ncallseqs = 0
                  , ncalls = 0
                  , totalGas = 0
                  , runningThreads = []
                  }

  flip runStateT initialState $ do
    flip evalRandT (mkStdGen effectiveSeed) $ do
      lift callback
      void $ replayCorpus vm initialCorpus
      run

  where
  run = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{stopOnFail, shrinkLimit} <- asks (.cfg.campaignConf)
    ncalls <- gets (.ncalls)

    let
      shrinkable test =
        case test.state of
          -- we shrink only tests which were solved on this
          -- worker, see 'updateOpenTest'
          Large n | test.workerId == Just workerId ->
            n < shrinkLimit
          _       -> False

      final test =
        case test.state of
          Solved   -> True
          Failed _ -> True
          _        -> False

      closeOptimizationTest test =
        case test.testType of
          OptimizationTest _ _ ->
            test { Test.state = Large 0
                 , workerId = Just workerId
                 }
          _ -> test

    if | stopOnFail && any final tests ->
         lift callback >> pure FastFailed

       -- we shrink first before going back to fuzzing
       | any shrinkable tests ->
         shrink >> lift callback >> run

       -- no shrinking work, fuzz
       | (null tests || any isOpen tests) && ncalls < testLimit ->
         fuzz >> lift callback >> run

       -- NOTE: this is a hack which forces shrinking of optimization tests
       -- after test limit is reached
       | ncalls >= testLimit && any (\t -> isOpen t && isOptimizationTest t) tests -> do
         liftIO $ forM_ testRefs $ \testRef ->
            atomicModifyIORef' testRef (\test -> (closeOptimizationTest test, ()))
         lift callback >> run

       -- no more work to do, means we reached the test limit, exit
       | otherwise ->
         lift callback >> pure TestLimitReached

  fuzz = randseq vm.env.contracts >>= fmap fst . callseq vm

  -- To avoid contention we only shrink tests that were falsified by this
  -- worker. Tests are marked with a worker in 'updateOpenTest'.
  --
  -- TODO: This makes some workers run longer as they work less on their
  -- test limit portion during shrinking. We should move to a test limit shared
  -- between workers to avoid that. This way other workers will "drain"
  -- the work queue.
  shrink = updateTests $ \test -> do
    if test.workerId == Just workerId then
      shrinkTest vm test
    else
      pure Nothing

-- | Generate a new sequences of transactions, either using the corpus or with
-- randomly created transactions
randseq
  :: (MonadRandom m, MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => Map (Expr 'EAddr) Contract
  -> m [Tx]
randseq deployedContracts = do
  env <- ask
  let world = env.world

  let
    mutConsts = env.cfg.campaignConf.mutConsts
    seqLen = env.cfg.campaignConf.seqLen

  -- TODO: include reproducer when optimizing
  --let rs = filter (not . null) $ map (.testReproducer) $ ca._tests

  -- Generate new random transactions
  randTxs <- replicateM seqLen (genTx world deployedContracts)
  -- Generate a random mutator
  cmut <- if seqLen == 1 then seqMutatorsStateless (fromConsts mutConsts)
                         else seqMutatorsStateful (fromConsts mutConsts)
  -- Fetch the mutator
  let mut = getCorpusMutation cmut
  corpus <- liftIO $ readIORef env.corpusRef
  if null corpus
    then pure randTxs -- Use the generated random transactions
    else mut seqLen corpus randTxs -- Apply the mutator

-- TODO callseq ideally shouldn't need to be MonadRandom

-- | Runs a transaction sequence and checks if any test got falsified or can be
-- minimized. Stores any useful data in the campaign state if coverage increased.
-- Returns resulting VM, as well as whether any new coverage was found.
callseq
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete RealWorld
  -> [Tx]
  -> m (VM Concrete RealWorld, Bool)
callseq vm txSeq = do
  env <- ask
  -- First, we figure out whether we need to execute with or without coverage
  -- optimization and gas info, and pick our execution function appropriately
  let
    conf = env.cfg.campaignConf
    coverageEnabled = isJust conf.knownCoverage
    execFunc = if coverageEnabled then execTxOptC else execTx

  -- Run each call sequentially. This gives us the result of each call
  -- and the new state
  (results, vm') <- evalSeq vm execFunc txSeq

  -- If there is new coverage, add the transaction list to the corpus
  newCoverage <- gets (.newCoverage)
  when newCoverage $ do
    ncallseqs <- gets (.ncallseqs)
    -- Even if this takes a bit of time, this is okay as finding new coverage
    -- is expected to be infrequent in the long term
    newSize <- liftIO $ atomicModifyIORef' env.corpusRef $ \corp ->
      -- Corpus is a bit too lazy, force the evaluation to reduce the memory usage
      let !corp' = force $ addToCorpus (ncallseqs + 1) results corp
      in (corp', corpusSize corp')

    (points, numCodehashes) <- liftIO $ coverageStats env.coverageRefInit env.coverageRefRuntime
    pushWorkerEvent NewCoverage { points
                                , numCodehashes
                                , corpusSize = newSize
                                , transactions = fst <$> results
                                }

  modify' $ \workerState ->

    let
      -- compute the addresses not present in the old VM via set difference
      newAddrs = Map.keys $ vm'.env.contracts \\ vm.env.contracts
      -- and construct a set to union to the constants table
      diffs = Map.fromList [(AbiAddressType, Set.fromList $ AbiAddress . forceAddr <$> newAddrs)]
      -- Now we try to parse the return values as solidity constants, and add them to 'GenDict'
      resultMap = returnValues results workerState.genDict.rTypes
      -- compute the new events to be stored
      eventDiffs = extractEventValues env.dapp vm vm'
      -- union the return results with the new addresses
      additions = Map.unionsWith Set.union [resultMap, eventDiffs, diffs]
      -- append to the constants dictionary
      updatedDict = workerState.genDict
        { constants = Map.unionWith Set.union workerState.genDict.constants additions
        , dictValues = Set.union (mkDictValues $ Set.unions $ Map.elems additions)
                                 workerState.genDict.dictValues
        }

    -- Update the worker state
    in workerState
      { genDict = updatedDict
        -- Reset the new coverage flag
      , newCoverage = False
        -- Keep track of the number of calls to `callseq`
      , ncallseqs = workerState.ncallseqs + 1
      }

  pure (vm', newCoverage)

  where
  -- Given a list of transactions and a return typing rule, checks whether we
  -- know the return type for each function called. If yes, try to parse the
  -- return value as a value of that type. Returns a 'GenDict' style Map.
  returnValues
    :: [(Tx, VMResult Concrete RealWorld)]
    -> (FunctionName -> Maybe AbiType)
    -> Map AbiType (Set AbiValue)
  returnValues txResults returnTypeOf =
    Map.unionsWith Set.union . mapMaybe extractValues $ txResults
    where
      extractValues (tx, result) = case result of
        VMSuccess (ConcreteBuf buf) -> do
          fname <- case tx.call of
            SolCall (fname, _) -> Just fname
            _ -> Nothing
          type' <- returnTypeOf fname
          case runGetOrFail (getAbi type') (LBS.fromStrict buf) of
            Right (_, _, abiValue) ->
              if isTuple type'
                then Just $ Map.fromListWith Set.union
                      [ (abiValueType val, Set.singleton val)
                      | val <- filter (/= AbiAddress (forceAddr cheatCode)) $ V.toList $ getTupleVector abiValue
                      ]
                else if abiValue /= AbiAddress (forceAddr cheatCode)
                  then Just $ Map.singleton type' (Set.singleton abiValue)
                  else Nothing
            _ -> Nothing
        _ -> Nothing

      isTuple (AbiTupleType _) = True
      isTuple _ = False
      getTupleVector (AbiTuple ts) = ts
      getTupleVector _ = error "Not a tuple!"

  -- | Add transactions to the corpus, discarding reverted ones
  addToCorpus :: Int -> [(Tx, VMResult Concrete RealWorld)] -> Corpus -> Corpus
  addToCorpus n res corpus =
    if null rtxs then corpus else Set.insert (n, rtxs) corpus
    where rtxs = fst <$> res

-- | Execute a transaction, capturing the PC and codehash of each instruction
-- executed, saving the transaction if it finds new coverage.
execTxOptC
  :: (MonadIO m, MonadReader Env m, MonadState WorkerState m, MonadThrow m)
  => VM Concrete RealWorld -> Tx
  -> m (VMResult Concrete RealWorld, VM Concrete RealWorld)
execTxOptC vm tx = do
  ((res, grew), vm') <- runStateT (execTxWithCov tx) vm
  when grew $ do
    modify' $ \workerState ->
      let
        dict' = case tx.call of
          SolCall c -> gaddCalls (Set.singleton c) workerState.genDict
          _ -> workerState.genDict
      in workerState { newCoverage = True, genDict = dict' }
  pure (res, vm')

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list
-- of transactions, constantly checking if we've solved any tests.
evalSeq
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete RealWorld -- ^ Initial VM
  -> (VM Concrete RealWorld -> Tx -> m (result, VM Concrete RealWorld))
  -> [Tx]
  -> m ([(Tx, result)], VM Concrete RealWorld)
evalSeq vm0 execFunc = go vm0 [] where
  go vm executedSoFar toExecute = do
    -- NOTE: we do reverse here because we build up this list by prepending,
    -- see the last line of this function.
    updateTests (updateOpenTest vm (reverse executedSoFar))
    modify' $ \workerState -> workerState { ncalls = workerState.ncalls + 1 }
    case toExecute of
      [] -> pure ([], vm)
      (tx:remainingTxs) -> do
        (result, vm') <- execFunc vm tx
        modify' $ \workerState -> workerState { totalGas = workerState.totalGas + fromIntegral (vm'.burned - vm.burned) }
        -- NOTE: we don't use the intermediate VMs, just the last one. If any of
        -- the intermediate VMs are needed, they can be put next to the result
        -- of each transaction - `m ([(Tx, result, VM)])`
        (remaining, vm'') <- go vm' (tx:executedSoFar) remainingTxs
        pure ((tx, result) : remaining, vm'')

-- | Update tests based on the return value from the given function.
-- Nothing skips the update.
updateTests
  :: (MonadIO m, MonadReader Env m, MonadState WorkerState m)
  => (EchidnaTest -> m (Maybe EchidnaTest))
  -> m ()
updateTests f = do
  testRefs <- asks (.testRefs)
  forM_ testRefs $ \testRef -> do
    test <- liftIO $ readIORef testRef
    f test >>= \case
      Just test' -> liftIO $ writeIORef testRef test'
      Nothing -> pure ()

findFailedTests
  :: (MonadIO m, MonadReader Env m, MonadState WorkerState m)
  => m [EchidnaTest]
findFailedTests = do
  testRefs <- asks (.testRefs)
  tests <- liftIO $ traverse readIORef testRefs
  pure $ filter didFail tests

-- | Update an open test after checking if it is falsified by the 'reproducer'
updateOpenTest
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete RealWorld -- ^ VM after applying potential reproducer
  -> [Tx] -- ^ potential reproducer
  -> EchidnaTest
  -> m (Maybe EchidnaTest)
updateOpenTest vm reproducer test = do
  case test.state of
    Open -> do
      (testValue, vm') <- checkETest test vm
      let result = getResultFromVM vm'
      case testValue of
        BoolValue False -> do
          workerId <- Just <$> gets (.workerId)
          let test' = test { Test.state = Large 0
                           , reproducer
                           , vm = Just vm
                           , result
                           , workerId
                           }
          pushWorkerEvent (TestFalsified test')
          pure $ Just test'

        IntValue value' | value' > value -> do
          let test' = test { reproducer
                           , value = IntValue value'
                           , vm = Just vm
                           , result
                           }
          pushWorkerEvent (TestOptimized test')
          pure $ Just test'
          where
          value =
            case test.value of
              IntValue x -> x
              -- TODO: fix this with proper types
              _ -> error "Invalid type of value for optimization"

        _ ->
          -- no luck with fuzzing this time
          pure Nothing
    _ ->
      -- not an open test, skip
      pure Nothing

-- | Listener reads events and runs the given 'handler' function. It exits after
-- receiving all 'WorkerStopped' events and sets the returned 'MVar' so the
-- parent thread can safely block on listener until all events are processed.
--
-- NOTE: because the 'Failure' event does not come from a specific fuzzing worker
-- it is possible that a listener won't process it if emitted after all workers
-- are stopped. This is quite unlikely and non-critical but should be addressed
-- in the long term.
spawnListener
  :: (MonadReader Env m, MonadIO m)
  => ((LocalTime, CampaignEvent) -> IO ())
  -- ^ a function that handles the events
  -> m (MVar ())
spawnListener handler = do
  cfg <- asks (.cfg)
  let nworkers = getNWorkers cfg.campaignConf
  eventQueue <- asks (.eventQueue)
  chan <- liftIO $ dupChan eventQueue
  stopVar <- liftIO newEmptyMVar
  liftIO $ void $ forkFinally (listenerLoop handler chan nworkers) (const $ putMVar stopVar ())
  pure stopVar

-- | Repeatedly run 'handler' on events from 'chan'.
-- Stops once 'workersAlive' workers stop.
listenerLoop
  :: (MonadIO m)
  => ((LocalTime, CampaignEvent) -> m ())
  -- ^ a function that handles the events
  -> Chan (LocalTime, CampaignEvent)
  -- ^ event channel
  -> Int
  -- ^ number of workers which have to stop before loop exits
  -> m ()
listenerLoop handler chan !workersAlive =
  when (workersAlive > 0) $ do
    event <- liftIO $ readChan chan
    handler event
    case event of
      (_, WorkerEvent _ _ (WorkerStopped _)) -> listenerLoop handler chan (workersAlive - 1)
      _                                      -> listenerLoop handler chan workersAlive
