module Echidna.Async where

import Control.Concurrent.Thread.Group (forkIO)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader, asks, ask)
import Control.Monad.State.Strict (MonadState, gets)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Time (LocalTime)

import Echidna.Types.Campaign (CampaignEvent, WorkerState(..))
import Echidna.Types.Config (Env(..))
import Echidna.Utility (getTimestamp)

spawnThread :: Env -> IO a -> IO ()
spawnThread env io = void $ forkIO env.threadGroup io

addEventHandler
  :: (MonadReader Env m, MonadIO m)
  => ((Int, LocalTime, CampaignEvent) -> IO ())
  -> m ()
addEventHandler f = do
  handlersRef <- asks (.eventHandlers)
  liftIO $ atomicModifyIORef' handlersRef (\l -> (f:l, ()))

pushEvent
  :: (MonadReader Env m, MonadState WorkerState m, MonadIO m)
  => CampaignEvent
  -> m ()
pushEvent event = do
  workerId <- gets (.workerId)
  env <- ask
  liftIO $ pushEventIO env workerId event

pushEventIO :: Env -> Int -> CampaignEvent -> IO ()
pushEventIO env workerId event = do
  time <- liftIO getTimestamp
  handlers <- readIORef env.eventHandlers
  mapM_ (\f -> spawnThread env $ f (workerId, time, event)) handlers
