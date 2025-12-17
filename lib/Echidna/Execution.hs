{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Echidna.Execution where

import Control.Concurrent.STM (atomically, writeTChan)
import Control.DeepSeq (force)
import Control.Monad (when, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom)
import Control.Monad.Reader (MonadReader, asks, liftIO, ask)
import Control.Monad.State.Strict
  (MonadState(..), StateT(..), gets, MonadIO, modify')
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (readIORef, atomicModifyIORef', writeIORef)
import Data.List qualified as List
import Data.Map (Map, (\\))
import Data.Map qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V

import EVM (cheatCode)
import EVM.ABI (getAbi, AbiType(AbiAddressType, AbiTupleType), AbiValue(AbiAddress, AbiTuple), abiValueType)
import EVM.Types (VM(..), VMResult(..), VMType(..), Expr(..))
import EVM.Types qualified as EVM

import Echidna.ABI
import Echidna.Events (extractEventValues)
import Echidna.Exec
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Corpus (Corpus, corpusSize)
import Echidna.Types.Coverage (coverageStats)
import Echidna.Types.InterWorker (WrappedMessage(..), Message(..), BroadcastMsg(NewCoverageInfo), AgentId(..))
import Echidna.SymExec.Symbolic (forceAddr)
import Echidna.Types.Signature (FunctionName)
import Echidna.Types.Test
import Echidna.Test (checkETest, getResultFromVM)
import Echidna.Types.Test qualified as Test
import Echidna.Types.Tx (TxCall(..), Tx(..))
import Echidna.Types.Worker (WorkerEvent(..))
import Echidna.Worker (pushWorkerEvent)

-- | Run all the transaction sequences from the corpus and accumulate campaign
-- state. Can be used to minimize corpus as the final campaign state will
-- contain minimized corpus without sequences that didn't increase the coverage.
replayCorpus
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete -- ^ VM to start replaying from
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

-- | Runs a transaction sequence and checks if any test got falsified or can be
-- minimized. Stores any useful data in the campaign state if coverage increased.
-- Returns resulting VM, as well as whether any new coverage was found.
callseq
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete
  -> [Tx]
  -> m (VM Concrete, Bool)
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

    -- Broadcast new coverage to other agents (e.g. Symbolic)
    workerId <- gets (.workerId)
    liftIO $ atomically $ writeTChan env.bus (WrappedMessage (FuzzerId workerId) (Broadcast (NewCoverageInfo points (fst <$> results))))

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
    :: [(Tx, VMResult Concrete)]
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
  addToCorpus :: Int -> [(Tx, VMResult Concrete)] -> Corpus -> Corpus
  addToCorpus n res corpus =
    if null rtxs then corpus else Set.insert (n, rtxs) corpus
    where rtxs = fst <$> res

-- | Execute a transaction, capturing the PC and codehash of each instruction
-- executed, saving the transaction if it finds new coverage.
execTxOptC
  :: (MonadIO m, MonadReader Env m, MonadState WorkerState m, MonadThrow m)
  => VM Concrete -> Tx
  -> m (VMResult Concrete, VM Concrete)
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
  => VM Concrete -- ^ Initial VM
  -> (VM Concrete -> Tx -> m (result, VM Concrete))
  -> [Tx]
  -> m ([(Tx, result)], VM Concrete)
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
  => VM Concrete -- ^ VM after applying potential reproducer
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
