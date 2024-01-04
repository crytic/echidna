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

import Echidna.Types.Campaign (CampaignEvent (..))
import Echidna.Types.Config (Env(..))

newtype SSE = SSE (Int, LocalTime, CampaignEvent)

instance ToJSON SSE where
  toJSON (SSE (workerId, time, event)) =
    object [ "worker" .= workerId
           , "timestamp" .= time
           , "data" .= event
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
          event@(_, _, campaignEvent) <- readChan sseChan
          let eventName = \case
                TestFalsified _ -> "test_falsified"
                TestOptimized _ -> "test_optimized"
                NewCoverage {} -> "new_coverage"
                TxSequenceReplayed _ _ -> "tx_sequence_replayed"
                WorkerStopped _ -> "worker_stopped"
          case campaignEvent of
            WorkerStopped _ -> do
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
