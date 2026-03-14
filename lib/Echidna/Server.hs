module Echidna.Server where

import Control.Concurrent
import Control.Monad (forever, when, void)
import Data.Aeson
import Data.Binary.Builder (fromLazyByteString)
import Data.IORef
import Data.Time (LocalTime)
import Data.Word (Word16)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)
import UnliftIO.STM (atomically, newBroadcastTChanIO, writeTChan, dupTChan, readTChan)

import Echidna.Types.Config (Env(..))
import Echidna.Types.Worker
import Echidna.Worker()
import Echidna.MCP (runMCPServer)

-- | T009: Spawn MCP server on specified port
-- Returns thread ID and starts MCP server using haskell-mcp-server
spawnMCPServer :: Word16 -> Env -> IO ThreadId
spawnMCPServer port env = do
  -- TODO: Pass actual workerRefs from campaign initialization
  -- For now use empty list - status tool will show 0 workers
  forkIO $ runMCPServer env [] (fromIntegral port) env.mcpCommandLog
  
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
  toJSON (SSE (time, ReproducerSaved filename)) =
    object [ "timestamp" .= time
           , "filename" .= filename
           ]
  toJSON (SSE (time, ServerLog msg)) =
    object [ "timestamp" .= time
           , "data" .= msg
           ]

runSSEServer :: MVar () -> Env -> Word16 -> Int -> IO ()
runSSEServer serverStopVar env port nworkers = do
  aliveRef <- newIORef nworkers
  sseChan <- dupChan env.eventQueue
  broadcastChan <- newBroadcastTChanIO

  void . forkIO . forever $ do
    event@(_, campaignEvent) <- readChan sseChan
    let eventName = \case
          WorkerEvent _ _ workerEvent ->
            case workerEvent of
              TestFalsified _ -> "test_falsified"
              TestOptimized _ -> "test_optimized"
              NewCoverage {} -> "new_coverage"
              SymExecLog _ -> "sym_exec_log"
              Log _ -> "log"
              SymExecError _ -> "sym_exec_error"
              TxSequenceReplayed {} -> "tx_sequence_replayed"
              TxSequenceReplayFailed {} -> "tx_sequence_replay_failed"
              WorkerStopped _ -> "worker_stopped"
          Failure _err -> "failure"
          ReproducerSaved _ -> "saved_reproducer"
          ServerLog _ -> "server_log"

    let serverEvent = ServerEvent
          { eventName = Just (eventName campaignEvent)
          , eventId = Nothing
          , eventData = [ fromLazyByteString $ encode (SSE event) ]
          }

    atomically $ writeTChan broadcastChan serverEvent

    case campaignEvent of
      WorkerEvent _ _ (WorkerStopped _) -> do
        aliveAfter <- atomicModifyIORef' aliveRef (\n -> (n-1, n-1))
        when (aliveAfter == 0) $ do
          atomically $ writeTChan broadcastChan CloseEvent
          putMVar serverStopVar ()
      _ -> pure ()

  let sseApp _req respond = do
        myChan <- atomically $ dupTChan broadcastChan
        let src = atomically $ readTChan myChan
        eventSourceAppIO src _req respond

  void . forkIO $ do
    run (fromIntegral port) sseApp
