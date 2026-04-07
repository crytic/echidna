{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Echidna.Campaign where

import Control.Concurrent (forkFinally, dupChan, readChan, newEmptyMVar, putMVar, MVar, Chan)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Time (LocalTime)

import Echidna.Types.Campaign () -- For instances if any
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Test (EchidnaTest(..), TestState(..))
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..))
import Echidna.Worker (getNWorkers)

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccessful :: [EchidnaTest] -> Bool
isSuccessful =
  all (\case { Passed -> True; Open -> True; _ -> False; } . (.state))

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
