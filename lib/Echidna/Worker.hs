module Echidna.Worker where

import Control.Concurrent
import Control.Monad (void, when)
import Control.Monad.Reader (MonadReader, MonadIO, asks, liftIO, ask)
import Control.Monad.State.Strict(MonadState(..), gets)
import Data.Aeson
import Data.Text (unpack)
import Data.Time (LocalTime)

import Echidna.ABI (encodeSig)
import Echidna.Types.Campaign
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Test
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Worker
import Echidna.Utility (getTimestamp)

-- | Number of workers, including SymExec worker if there is one
getNWorkers :: CampaignConf -> Int
getNWorkers conf = getNFuzzWorkers conf + (if conf.symExec then 1 else 0)

workerIDToType :: CampaignConf -> WorkerId -> WorkerType
workerIDToType conf wid = if conf.symExec && wid == (getNWorkers conf - 1) then SymbolicWorker else FuzzWorker

instance ToJSON WorkerEvent where
  toJSON = \case
    TestFalsified test -> toJSON test
    TestOptimized test -> toJSON test
    NewCoverage { points, numCodehashes, corpusSize } ->
      object [ "coverage" .= points, "contracts" .= numCodehashes, "corpus_size" .= corpusSize]
    SymExecError msg -> object [ "msg" .= msg ]
    SymExecLog msg -> object [ "msg" .= msg ]
    Log msg -> object [ "msg" .= msg ]
    TxSequenceReplayed file current total ->
      object [ "file" .= file, "current" .= current, "total" .= total ]
    TxSequenceReplayFailed file tx ->
      object [ "file" .= file, "tx" .= tx ]
    WorkerStopped reason -> object [ "reason" .= show reason ]

pushWorkerEvent
  :: (MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => WorkerEvent
  -> m ()
pushWorkerEvent event = do
  workerId <- gets (.workerId)
  env <- ask
  let workerType = workerIDToType env.cfg.campaignConf workerId
  liftIO $ pushCampaignEvent env (WorkerEvent workerId workerType event)

pushCampaignEvent :: Env -> CampaignEvent -> IO ()
pushCampaignEvent env event = do
  time <- liftIO getTimestamp
  writeChan env.eventQueue (time, event)

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

ppCampaignEvent :: CampaignEvent -> String
ppCampaignEvent = \case
  WorkerEvent _ _ e -> ppWorkerEvent e
  Failure err -> err
  ReproducerSaved f -> "Saved reproducer to " <> f
  ServerLog msg -> msg

ppWorkerEvent :: WorkerEvent -> String
ppWorkerEvent = \case
  TestFalsified test ->
    "Test " <> unpack (showTest test) <> " falsified!"
  TestOptimized test ->
    let name = case test.testType of OptimizationTest n _ -> n; _ -> error "fixme"
    in "New maximum value of " <> unpack name <> ": " <> show test.value
  NewCoverage { points, numCodehashes, corpusSize, transactions } ->
    let -- the coverage is credited to the last transaction of the sequence
        culprit = case transactions of
          [] -> "init"
          txs -> let tx = last txs in case tx.call of
            SolCall (name, _) -> unpack name
            SolCreate _ -> "constructor"
            SolCalldata _ -> "fallback"
            NoCall -> "no call"
    in "New coverage: " <> show points <> " instr, "
      <> show numCodehashes <> " contracts, "
      <> show corpusSize <> " seqs in corpus (" <> culprit <> ")"
  SymExecError err ->
    "Symbolic execution failed: " <> err
  SymExecLog msg ->
    "Symbolic execution log: " <> msg
  Log msg ->
    msg
  TxSequenceReplayed file current total ->
    "Sequence replayed from corpus file " <> file <> " (" <> show current <> "/" <> show total <> ")"
  TxSequenceReplayFailed file tx ->
    "WARNING: Sequence replay from corpus file " <> file <> " failed. " <>
    "The destination contract is not deployed for this transaction: " <> show tx <> ". " <>
    "Remove the file or the transaction to fix the issue."
  WorkerStopped TestLimitReached ->
    "Test limit reached. Stopping."
  WorkerStopped SymbolicExplorationDone ->
    "Symbolic worker ran out of transactions to explore. Stopping."
  WorkerStopped SymbolicVerificationDone ->
    "Symbolic worker finished with the list of methods to verify. Stopping."
  WorkerStopped TimeLimitReached ->
    "Time limit reached. Stopping."
  WorkerStopped FastFailed ->
    "A test was falsified. Stopping."
  WorkerStopped (Killed e) ->
    "Killed (" <> e <>"). Stopping."
  WorkerStopped (Crashed e) ->
    "Crashed:\n\n" <>
    e <>
    "\n\nPlease report it to https://github.com/crytic/echidna/issues"
  where
    showTest test = case test.testType of
      PropertyTest n _ -> n
      AssertionTest _ n _ -> encodeSig n
      CallTest n _ -> n
      _ -> error "impossible"
