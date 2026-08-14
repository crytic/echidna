{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Echidna.Worker.Fuzz (runFuzzWorker) where

import Control.Concurrent.STM (atomically, dupTChan)
import Control.Monad (forM_, replicateM, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, evalRandT, getRandom)
import Control.Monad.Reader (MonadReader, ask, asks, liftIO)
import Control.Monad.State.Strict (MonadIO, MonadState, StateT, gets, runStateT)
import Control.Monad.Trans (lift)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import System.Random (mkStdGen)

import EVM.Types hiding (Env, Frame(state), Gas)

import Echidna.ABI
import Echidna.Mutator.Corpus
import Echidna.Orphans.Rand ()
import Echidna.Shrink (isShrinkable, shrinkWorkerTests)
import Echidna.Transaction
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Random (rElem)
import Echidna.Types.Test
import Echidna.Types.Test qualified as Test
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker
import Echidna.Worker.Command (checkMessages)
import Echidna.Worker.Sequence (callseq, replayCorpus)

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
  -> m (WorkerStopReason, WorkerState)
runFuzzWorker callback vm dict workerId initialCorpus testLimit = do
  bus <- asks (.bus)
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
      -- Each worker reads the bus through its own duplicate, so a message
      -- reaches every worker instead of being raced for by whoever gets there
      -- first.
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

-- | Generate a new sequence of transactions: from one of the sequences
-- prioritized over the bus, or, failing that, the standard way.
randseq
  :: (MonadRandom m, MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => Map (Expr 'EAddr) Contract
  -> m [Tx]
randseq deployedContracts = do
  prioritized <- gets (.prioritizedSequences)
  case NE.nonEmpty prioritized of
    Nothing -> genStandardSeq deployedContracts
    Just seqs -> do
      -- Pick one of the prioritized sequences, then use it only with the
      -- probability it was injected with.
      (prob, prototypes) <- rElem seqs
      roll <- getRandom
      if roll <= prob
        then genPrioritizedSeq deployedContracts prototypes
        else genStandardSeq deployedContracts

-- | Generate a new sequence of transactions, either using the corpus or with
-- randomly created transactions
genStandardSeq
  :: (MonadRandom m, MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => Map (Expr 'EAddr) Contract
  -> m [Tx]
genStandardSeq deployedContracts = do
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
