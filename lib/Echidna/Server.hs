module Echidna.Server where

import Control.Concurrent
import Control.Monad (when, void)
import Data.Aeson
import Data.Binary.Builder (fromLazyByteString)
import Data.IORef
import Data.Map qualified as Map
import Data.Time (LocalTime)
import Data.Word (Word16)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, responseLBS, pathInfo, requestMethod)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)

import EVM.Dapp (DappInfo(..))

import Echidna.Output.Source (saveLcovHook)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Worker()
import Echidna.Types.Worker

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
                    SymExecLog _ -> "sym_exec_log"
                    SymExecError _ -> "sym_exec_error"
                    TxSequenceReplayed {} -> "tx_sequence_replayed"
                    TxSequenceReplayFailed {} -> "tx_sequence_replay_failed"
                    WorkerStopped _ -> "worker_stopped"
                Failure _err -> "failure"
                ReproducerSaved _ -> "saved_reproducer"
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

  let sseApp = eventSourceAppIO sseListener

  let app :: Application
      app req respond = case (requestMethod req, pathInfo req) of
        -- SSE endpoint
        ("GET", ["events"]) -> sseApp req respond

        -- Dump LCOV coverage
        ("POST", ["dump_lcov"]) -> do
          case env.cfg.campaignConf.corpusDir of
            Nothing ->
              respond $ responseLBS status404 [("Content-Type", "application/json")]
                "{\"error\":\"No corpus directory configured\"}"
            Just dir -> do
              let contracts = Map.elems env.dapp.solcByName
              fn <- saveLcovHook env dir env.sourceCache contracts
              respond $ responseLBS status200 [("Content-Type", "application/json")]
                (encode $ object ["file" .= fn])

        -- Unknown endpoint
        _ -> respond $ responseLBS status404 [("Content-Type", "application/json")]
              "{\"error\":\"Not found\"}"

  void . forkIO $ do
    run (fromIntegral port) app
