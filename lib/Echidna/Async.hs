module Echidna.Async where

import Control.Concurrent (threadDelay, forkFinally)
import Control.Monad (void, when)
import Control.Monad.Reader (MonadReader, asks, ask)
import Control.Monad.State.Strict (MonadState, gets)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (atomicModifyIORef', readIORef)
import Data.Time (LocalTime)

import Echidna.Types.Campaign (CampaignEvent, WorkerState(..))
import Echidna.Types.Config (Env(..))
import Echidna.Utility (getTimestamp)

awaitThreads :: Env -> IO ()
awaitThreads env = do
  n <- readIORef env.numUnfinishedThreads
  when (n > 0) $ threadDelay 10_000 >> awaitThreads env

spawnThread :: Env -> IO () -> IO ()
spawnThread env io = do
  atomicModifyIORef' env.numUnfinishedThreads (\n -> (n+1, ()))
  void $ forkFinally io (const $ atomicModifyIORef' env.numUnfinishedThreads (\n -> (n-1, ())))

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
  mapM_ ($ (workerId, time, event)) handlers
