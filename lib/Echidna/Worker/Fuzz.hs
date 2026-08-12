{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Echidna.Worker.Fuzz (FuzzerAgent(..), runFuzzWorker) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (atomically, dupTChan, putTMVar, tryReadTChan)
import Control.Monad (foldM, forM_, replicateM, void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, evalRandT, getRandom, getRandomR)
import Control.Monad.Reader (MonadReader, ask, asks, liftIO, runReaderT)
import Control.Monad.State.Strict (MonadIO, MonadState, StateT, get, gets, modify', runStateT)
import Control.Monad.Trans (lift)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (getCurrentDirectory)
import System.Random (mkStdGen)

import EVM.ABI (AbiValue)
import EVM.Dapp (DappInfo(..))
import EVM.Format (showTraceTree)
import EVM.Types hiding (Env, Frame(state), Gas, Log)
import EVM.Types qualified as EVM

import Echidna.ABI
import Echidna.Events (extractEvents)
import Echidna.Exec (execTx)
import Echidna.Mutator.Corpus
import Echidna.Orphans.Rand ()
import Echidna.Output.Source (saveLcovHook)
import Echidna.Shrink (isShrinkable, shrinkWorkerTests)
import Echidna.Transaction
import Echidna.Types.Agent
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.InterWorker
  (AgentId(..), Bus, FuzzerCmd(..), Message(..), WrappedMessage(..))
import Echidna.Types.Random (rElem)
import Echidna.Types.Test
import Echidna.Types.Test qualified as Test
import Echidna.Types.Tx (Tx, TxResult(..), getResult)
import Echidna.Types.Worker
import Echidna.UI.Report (ppTx)
import Echidna.Worker (pushCampaignEvent)
import Echidna.Worker.Sequence (callseq, replayCorpus)

-- | A fuzzing worker, driven by the inter-worker bus.
data FuzzerAgent = FuzzerAgent
  { fuzzerId :: Int
  , initialVm :: VM Concrete
  , initialDict :: GenDict
  , initialCorpus :: [(FilePath, [Tx])]
  , testLimit :: Int
  , stateRef :: IORef WorkerState
  }

instance Show FuzzerAgent where
  show agent = "FuzzerAgent { fuzzerId = " ++ show agent.fuzzerId ++ " }"

instance Agent FuzzerAgent where
  getAgentId agent = FuzzerId agent.fuzzerId

  runAgent agent bus env = do
    let workerId = agent.fuzzerId
        -- Publish the worker state so the UI and the MCP server can read it
        callback = get >>= liftIO . writeIORef agent.stateRef

    pushCampaignEvent env $ WorkerEvent workerId FuzzWorker
      (Log ("Starting FuzzerAgent " ++ show workerId))

    (reason, _) <- flip runReaderT env $
      runFuzzWorker callback agent.initialVm agent.initialDict workerId
                    agent.initialCorpus agent.testLimit bus

    pure reason

-- | Run a fuzzing campaign given an initial universe state, some tests, and an
-- optional dictionary to generate calls with. Return the 'Campaign' state once
-- we can't solve or shrink anything.
runFuzzWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> [(FilePath, [Tx])]
  -- ^ Initial corpus of transactions
  -> Int     -- ^ Test limit for this worker
  -> Bus     -- ^ Inter-worker bus
  -> m (WorkerStopReason, WorkerState)
runFuzzWorker callback vm dict workerId initialCorpus testLimit bus = do
  let
    effectiveSeed = dict.defSeed + workerId
    initialState =
      initialWorkerState { workerId
                         , genDict = dict { defSeed = effectiveSeed }
                         }

  flip runStateT initialState $ do
    flip evalRandT (mkStdGen effectiveSeed) $ do
      lift callback
      void $ replayCorpus vm initialCorpus
      -- Each worker reads the bus through its own duplicate, so a message is
      -- delivered to every worker instead of being raced for.
      chan <- liftIO $ atomically $ dupTChan bus
      run chan

  where
  run chan = do
    checkMessages chan
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{stopOnFail, shrinkLimit} <- asks (.cfg.campaignConf)
    ncalls <- gets (.ncalls)

    let
      closeOptimizationTest test =
        case test.testType of
          OptimizationTest _ _ ->
            test { Test.state = Large 0
                 , workerId = Just workerId
                 }
          _ -> test

    if | stopOnFail && any isConclusiveFailure tests ->
         lift callback >> pure FastFailed

       -- we shrink first before going back to fuzzing
       | any (isShrinkable shrinkLimit workerId) tests ->
         shrink >> lift callback >> run chan

       -- no shrinking work, fuzz
       | (null tests || any isOpen tests) && ncalls < testLimit ->
         fuzz >> lift callback >> run chan

       -- Test limit reached. Close any open optimization tests so they
       -- enter the shrink loop above, same as other test types.
       | ncalls >= testLimit && any (\t -> isOpen t && isOptimizationTest t) tests -> do
         liftIO $ forM_ testRefs $ \testRef ->
            atomicModifyIORef' testRef (\test -> (closeOptimizationTest test, ()))
         lift callback >> run chan

       -- no more work to do, exit
       | otherwise ->
         lift callback >> pure TestLimitReached

  fuzz = randseq vm.env.contracts >>= \txs -> fst <$> callseq vm txs False

  -- TODO: Shrinking only this worker's tests makes some workers run longer as
  -- they work less on their test limit portion during shrinking. We should move
  -- to a test limit shared between workers to avoid that. This way other
  -- workers will "drain" the work queue.
  shrink = shrinkWorkerTests workerId vm

  -- Handle at most one bus message per loop iteration, so that message
  -- handling never starves fuzzing.
  checkMessages chan = do
    msg <- liftIO $ atomically $ tryReadTChan chan
    case msg of
      Just (WrappedMessage _ (ToFuzzer tid cmd)) | tid == workerId -> handleCmd cmd
      _ -> pure ()

  handleCmd (SolutionFound _) =
    -- Received help from the symbolic worker; the transactions reach us
    -- through the corpus, so there is nothing to do here yet.
    pure ()

  handleCmd DumpLcov = do
    env <- ask
    liftIO $ do
      let contracts = Map.elems env.dapp.solcByName
      dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
      void $ saveLcovHook env dir env.sourceCache contracts
      putStrLn $ "Fuzzer " ++ show workerId ++ ": dumped LCOV coverage."

  handleCmd (FuzzSequence txs prob) =
    modify' $ \s -> s { prioritizedSequences = (prob, txs) : s.prioritizedSequences }

  handleCmd ClearPrioritization =
    modify' $ \s -> s { prioritizedSequences = [] }

  handleCmd (ExecuteSequence txs trace replyVar) =
    -- Only worker 0 responds; tid is expected to be 0 from MCP.
    when (workerId == 0) $ do
      report <- executeSeq trace vm txs
      liftIO $ atomically $ putTMVar replyVar report

  handleCmd (EnableSampling sig) =
    modify' $ \s ->
      if Map.size s.sampledFunctions >= maxSampledFunctions
         || Map.member sig s.sampledFunctions
      then s
      else s { sampledFunctions = Map.insert sig emptySampleStats s.sampledFunctions }

  handleCmd ClearSampling =
    modify' $ \s -> s { sampledFunctions = Map.empty }

-- | Replay a concrete sequence and return a compact JSON report.
--   Uses 'execTx' directly so the running fuzzing campaign is not perturbed
--   (no coverage/corpus side effects).
--   When @includeTrace@ is 'True' the report additionally carries a @trace@
--   field with the EVM trace tree of the LAST tx (intermediate trees are
--   skipped to keep cost bounded — 'showTraceTree' is expensive).
executeSeq
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => Bool -> VM Concrete -> [Tx] -> m String
executeSeq includeTrace vm0 txs = do
  dapp <- asks (.dapp)
  let step (acc, mbFailed, mbFailedStatus, vm) (i, tx) = do
        let burnedBefore = vm.burned
        (vmResult, vm') <- execTx vm tx
        let txResult = getResult vmResult
        txStr <- ppTx vm' False tx
        let events = extractEvents True dapp vm'
            status = txStatus txResult events
            failed       = if status == "completed" then mbFailed       else mbFailed       <|> Just i
            failedStatus = if status == "completed" then mbFailedStatus else mbFailedStatus <|> Just status
            gasUsed = vm'.burned - burnedBefore
            entry = object
              [ "index"    .= i
              , "call"     .= txStr
              , "status"   .= status
              , "result"   .= show txResult
              , "gas_used" .= gasUsed
              , "logs"     .= map T.unpack events
              ]
        pure (entry : acc, failed, failedStatus, vm')
  (entries, mbFailed, mbFailedStatus, finalVm) <-
      foldM step ([], Nothing, Nothing, vm0) (zip [1 :: Int ..] txs)
  let overall = case mbFailedStatus of
        Just "assertion_failed" -> "assertion_failed" :: String
        Just _                  -> "reverted"
        Nothing                 -> "completed"
      baseFields =
        [ "status"             .= overall
        , "transaction_count"  .= length txs
        , "failed_tx_index"    .= mbFailed
        , "final_block_number" .= show (EVM.forceLit finalVm.block.number)
        , "final_timestamp"    .= show (EVM.forceLit finalVm.block.timestamp)
        , "transactions"       .= reverse entries
        ]
      traceField
        | includeTrace && not (null txs) =
            [ "trace" .= T.unpack (showTraceTree dapp finalVm) ]
        | otherwise = []
  pure $ BL8.unpack $ encode $ object (baseFields ++ traceField)

txStatus :: TxResult -> [Text] -> String
txStatus result events
  | any isAssertionLog events                     = "assertion_failed"
  | result `elem` [ReturnTrue, ReturnFalse, Stop] = "completed"
  | otherwise                                     = "reverted"

isAssertionLog :: Text -> Bool
isAssertionLog event =
  "AssertFail" `T.isInfixOf` event || "Panic(AbiUInt 256 1)" `T.isInfixOf` event

-- | Generate a new sequence of transactions: either from a sequence
-- prioritized over the bus, or with the standard corpus mutators.
randseq
  :: (MonadRandom m, MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => Map (Expr 'EAddr) Contract
  -> m [Tx]
randseq deployedContracts = do
  prioritized <- gets (.prioritizedSequences)

  mbSeq <- if null prioritized
           then pure Nothing
           else do
             -- Select a prioritized sequence based on probability
             (prob, seqPrototype) <- rElem (NE.fromList prioritized)
             useIt <- (<= prob) <$> getRandom
             pure $ if useIt then Just seqPrototype else Nothing

  case mbSeq of
    Just seqPrototype -> genPrioritizedSeq deployedContracts seqPrototype
    Nothing -> genStandardSeq deployedContracts

-- | Generate a sequence of transactions based on a prioritized prototype
genPrioritizedSeq
  :: (MonadRandom m, MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => Map (Expr 'EAddr) Contract
  -> [(Text, [Maybe AbiValue])]
  -> m [Tx]
genPrioritizedSeq deployedContracts seqPrototype = do
  env <- ask
  let world = env.world
      seqLen = env.cfg.campaignConf.seqLen

  -- Expand the prototype into concrete transactions
  let expandPrototype [] = return []
      expandPrototype [p] = do
          tx <- genTxFromPrototype world deployedContracts p
          return [tx]
      expandPrototype (p:ps) = do
          tx <- genTxFromPrototype world deployedContracts p
          -- Insert random transactions between prototype transactions to
          -- increase fuzzing diversity
          n <- getRandomR (0, 3)
          rndTxs <- replicateM n (genTx world deployedContracts)
          rest <- expandPrototype ps
          return ((tx : rndTxs) ++ rest)

  expandedTxs <- expandPrototype seqPrototype
  corpusSet <- liftIO $ readIORef env.corpusRef
  wid <- gets (.workerId)

  -- Select a prefix from the existing corpus. Worker 0 always uses an empty
  -- prefix, so the prototype is exercised from the initial state too.
  prefix <- if Set.null corpusSet || wid == 0
            then pure []
            else do
              idx <- getRandomR (0, Set.size corpusSet - 1)
              let (_, cTxs) = Set.elemAt idx corpusSet
                  middleLen = length expandedTxs
                  maxPrefix = seqLen - middleLen
              if maxPrefix <= 0
                then pure []
                else do
                  k <- getRandomR (0, min (length cTxs) maxPrefix)
                  pure (take k cTxs)

  let combined = prefix ++ expandedTxs
      len = length combined

  -- Pad with random transactions if the sequence is too short
  if len < seqLen
    then do
      paddingTxs <- replicateM (seqLen - len) (genTx world deployedContracts)
      pure (combined ++ paddingTxs)
    else
      pure (take seqLen combined)

-- | Generate a sequence of transactions using standard fuzzing techniques
genStandardSeq
  :: (MonadRandom m, MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => Map (Expr 'EAddr) Contract
  -> m [Tx]
genStandardSeq deployedContracts = do
  env <- ask
  let world = env.world
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
