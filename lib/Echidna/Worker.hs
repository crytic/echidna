module Echidna.Worker where

import Control.Concurrent
import Control.Monad.Reader (MonadReader, MonadIO, liftIO, ask)
import Control.Monad.State.Strict(MonadState(..), gets)
import Data.Aeson
import Data.Text (unpack)

import Echidna.ABI (encodeSig)
import Echidna.Types.Test
import Echidna.Types.Campaign
import Echidna.Types.Config (Env(..), EConfig(..))
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

ppCampaignEvent :: CampaignEvent -> String
ppCampaignEvent = \case
  WorkerEvent _ _ e -> ppWorkerEvent e
  Failure err -> err
  ReproducerSaved f -> "Saved reproducer to " <> f

ppWorkerEvent :: WorkerEvent -> String
ppWorkerEvent = \case
  TestFalsified test ->
    "Test " <> unpack (showTest test) <> " falsified!"
  TestOptimized test ->
    let name = case test.testType of OptimizationTest n _ -> n; _ -> error "fixme"
    in "New maximum value of " <> unpack name <> ": " <> show test.value
  NewCoverage { points, numCodehashes, corpusSize } ->
    "New coverage: " <> show points <> " instr, "
      <> show numCodehashes <> " contracts, "
      <> show corpusSize <> " seqs in corpus"
  SymExecError err ->
    "Symbolic execution failed: " <> err
  SymExecLog msg ->
    "Symbolic execution log: " <> msg
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
