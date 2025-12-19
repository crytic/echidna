{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiWayIf #-}

module Echidna.Agent.Fuzzer where

import Control.Concurrent.STM (atomically, tryReadTChan, dupTChan, putTMVar)
import Control.Monad (replicateM, void, forM_, when)
import Control.Monad.Reader (runReaderT, liftIO, asks, MonadReader, ask)
import Control.Monad.State.Strict (runStateT, get, gets, modify', MonadState)
import Control.Monad.Random.Strict (evalRandT, MonadRandom, RandT)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO)
import System.Random (mkStdGen)
import Data.IORef (IORef, writeIORef, readIORef, atomicModifyIORef')
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (getCurrentDirectory)

import Echidna.Output.Source (saveLcovHook)
import EVM.Dapp (DappInfo(..))
import EVM.Types (VM(..), VMType(Concrete), Expr(..), EType(..), Contract)
import qualified EVM.Types as EVM

import Echidna.ABI (GenDict(..))
import Echidna.Execution (replayCorpus, callseq, updateTests)
import Echidna.Mutator.Corpus (getCorpusMutation, seqMutatorsStateless, seqMutatorsStateful, fromConsts)
import Echidna.Shrink (shrinkTest)
import Echidna.Transaction (genTx)
import Echidna.Types.Agent
import Echidna.Types.Campaign (WorkerState(..), CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.InterWorker (AgentId(..), Bus, WrappedMessage(..), Message(..), FuzzerCmd(..))
import Echidna.Types.Test (EchidnaTest(..), TestState(..), TestType(..), isOpen, isOptimizationTest)
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker (WorkerEvent(..), WorkerType(..), CampaignEvent(..), WorkerStopReason(..))
import qualified Echidna.Types.Worker as Worker
import Echidna.Worker (pushCampaignEvent)

instance (MonadThrow m) => MonadThrow (RandT g m) where
  throwM = lift . throwM

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
        vm = agent.initialVm
        dict = agent.initialDict
        corpus = agent.initialCorpus
        limit = agent.testLimit
        ref = agent.stateRef

        effectiveSeed = dict.defSeed + workerId
        effectiveGenDict = dict { defSeed = effectiveSeed }

        initialState = WorkerState
          { workerId
          , genDict = effectiveGenDict
          , newCoverage = False
          , ncallseqs = 0
          , ncalls = 0
          , totalGas = 0
          , runningThreads = []
          , prioritizedFunctions = []
          }

    -- Callback to update the IORef with the current state
    let callback = get >>= liftIO . writeIORef ref

    (reason, _) <- flip evalRandT (mkStdGen effectiveSeed) $
           flip runReaderT env $
             flip runStateT initialState $ do
               liftIO $ pushCampaignEvent env (WorkerEvent workerId FuzzWorker (Worker.Log ("Starting FuzzerAgent " ++ show workerId)))
               callback
               void $ replayCorpus vm corpus
               workerChan <- liftIO $ atomically $ dupTChan bus
               fuzzerLoop callback vm limit workerChan

    liftIO $ pushCampaignEvent env (WorkerEvent workerId FuzzWorker (WorkerStopped reason))

    return ()

fuzzerLoop
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => m () -- ^ Callback
  -> VM Concrete
  -> Int -- ^ Test limit
  -> Bus
  -> m WorkerStopReason
fuzzerLoop callback vm testLimit bus = do
  -- Check for messages
  -- TODO: Properly handle messages. For now we just check if we should stop?
  -- But runAgent doesn't return until done.

  -- We can peek the bus. But standard fuzzer might be busy.
  -- Maybe check bus every N iterations?

  run
  where
  run = do
    checkMessages

    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{stopOnFail, shrinkLimit} <- asks (.cfg.campaignConf)
    ncalls <- gets (.ncalls)
    workerId <- gets (.workerId)

    let
      shrinkable test =
        case test.state of
          -- we shrink only tests which were solved on this
          -- worker
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
            test { state = Large 0
                 , workerId = Just workerId
                 }
          _ -> test

    if | stopOnFail && any final tests ->
         callback >> pure FastFailed

       -- we shrink first before going back to fuzzing
       | any shrinkable tests ->
         shrink >> callback >> run

       -- no shrinking work, fuzz
       | (null tests || any isOpen tests) && ncalls < testLimit ->
         fuzz >> callback >> run

       -- NOTE: this is a hack which forces shrinking of optimization tests
       -- after test limit is reached
       | ncalls >= testLimit && any (\t -> isOpen t && isOptimizationTest t) tests -> do
         liftIO $ forM_ testRefs $ \testRef ->
            atomicModifyIORef' testRef (\test -> (closeOptimizationTest test, ()))
         callback >> run

       -- no more work to do, means we reached the test limit, exit
       | otherwise ->
         callback >> pure TestLimitReached

  fuzz = randseq vm.env.contracts >>= fmap fst . (\txs -> callseq vm txs False)

  shrink = do
    wid <- gets (.workerId)
    updateTests $ \test -> do
      if test.workerId == Just wid then
        shrinkTest vm test
      else
        pure Nothing

  checkMessages = do
     -- Non-blocking read
     msg <- liftIO $ atomically $ tryReadTChan bus
     case msg of
       Just (WrappedMessage _ (ToFuzzer tid (SolutionFound _))) -> do
          workerId <- gets (.workerId)
          when (tid == workerId) $ do
             -- Received help!
             pure ()
       Just (WrappedMessage _ (ToFuzzer tid DumpLcov)) -> do
          workerId <- gets (.workerId)
          when (tid == workerId) $ do
            env <- ask
            liftIO $ do
               let contracts = Map.elems env.dapp.solcByName
               dir <- maybe getCurrentDirectory pure env.cfg.campaignConf.corpusDir
               void $ saveLcovHook env dir env.sourceCache contracts
               putStrLn $ "Fuzzer " ++ show workerId ++ ": dumped LCOV coverage."
            pure ()
       Just (WrappedMessage _ (ToFuzzer _ (PrioritizeFunction _))) -> do
          -- Deprecated
          pure ()
       Just (WrappedMessage _ (ToFuzzer tid (FuzzTransaction funcName args))) -> do
          workerId <- gets (.workerId)
          when (tid == workerId) $ do
             modify' $ \s -> s { prioritizedFunctions = (funcName, args) : s.prioritizedFunctions }
             pure ()
       Just (WrappedMessage _ (ToFuzzer tid ClearPrioritization)) -> do
          workerId <- gets (.workerId)
          when (tid == workerId) $ do
             modify' $ \s -> s { prioritizedFunctions = [] }
             pure ()
       Just (WrappedMessage _ (ToFuzzer tid (ExecuteSequence txs replyVar))) -> do
          workerId <- gets (.workerId)
          when (tid == workerId) $ do
             (_, newCov) <- callseq vm txs False
             liftIO $ case replyVar of
                Just var -> atomically $ putTMVar var newCov
                Nothing -> pure ()
             pure ()
       _ -> pure ()

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
