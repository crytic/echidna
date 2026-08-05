module Echidna.Campaign where

import Control.Concurrent (MVar, dupChan, forkFinally, newEmptyMVar, putMVar)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Control.Monad.State.Strict (StateT)
import Data.Text (Text)
import Data.Time (LocalTime)

import EVM.Types hiding (Env)

import Echidna.ABI (GenDict)
import Echidna.Agent.Fuzzer (runFuzzWorker)
import Echidna.Agent.Symbolic (runSymWorker)
import Echidna.Execution (listenerLoop)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Test
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker
import Echidna.Worker (getNWorkers)

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccessful :: [EchidnaTest] -> Bool
isSuccessful =
  all (\case { Passed -> True; Open -> True; _ -> False; } . (.state))

runWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => WorkerType
  -> StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete -- ^ Initial VM state
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
