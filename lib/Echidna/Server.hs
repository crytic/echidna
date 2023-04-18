module Echidna.Server where

import Control.Concurrent
import Control.Monad (when, void)
import Data.Aeson
import Data.Binary.Builder (fromLazyByteString)
import Data.IORef
import Data.Time (LocalTime)
import Data.Word (Word16)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)

import Echidna.Types.Campaign
import Echidna.Types.Config (Env(..))

newtype SSE = SSE (LocalTime, CampaignEvent)

instance ToJSON SSE where
  toJSON (SSE (time, WorkerEvent workerId workerType event)) =
    object [ "worker" .= workerId
           , "workerType" .= workerTypeString workerType
           , "timestamp" .= time
           , "data" .= event
           ]
    where
      workerTypeString :: WorkerType -> String
      workerTypeString SymbolicWorker = "symbolic"
      workerTypeString FuzzWorker = "fuzz"
  toJSON (SSE (time, Failure reason)) =
    object [ "timestamp" .= time
           , "data" .= reason
           ]

runSSEServer :: MVar () -> Env -> Word16 -> Int -> IO ()
runSSEServer serverStopVar env port nworkers = do
  aliveRef <- newIORef nworkers
  sseChan <- dupChan env.eventQueue

  let sseListener = do
        aliveNow <- readIORef aliveRef
        if aliveNow == 0 then
          pure CloseEvent
        else do
          event@(_, campaignEvent) <- readChan sseChan
          let eventName = \case
                WorkerEvent _ _ workerEvent ->
                  case workerEvent of
                    TestFalsified _ -> "test_falsified"
                    TestOptimized _ -> "test_optimized"
                    NewCoverage {} -> "new_coverage"
                    SymNoNewCoverage -> "sym_no_new_coverage"
                    TxSequenceReplayed {} -> "tx_sequence_replayed"
                    TxSequenceReplayFailed {} -> "tx_sequence_replay_failed"
                    WorkerStopped _ -> "worker_stopped"
                Failure _err -> "failure"
          case campaignEvent of
            WorkerEvent _ _ (WorkerStopped _) -> do
              aliveAfter <- atomicModifyIORef' aliveRef (\n -> (n-1, n-1))
              when (aliveAfter == 0) $ putMVar serverStopVar ()
            _ -> pure ()
          pure $ ServerEvent
            { eventName = Just (eventName campaignEvent)
            , eventId = Nothing
            , eventData = [ fromLazyByteString $ encode (SSE event) ]
            }

  void . forkIO $ do
    run (fromIntegral port) $ eventSourceAppIO sseListener
