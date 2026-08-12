-- | Read and update the campaign's set of tests, held in 'Env.testRefs'.
--
-- These operations are worker-agnostic: they act on the shared test set and
-- never touch 'WorkerState'. Worker-scoped test updates -- the ones stamping a
-- test with the worker that falsified it, or emitting worker events -- live
-- next to their caller in 'Echidna.Worker.Sequence'.
module Echidna.Test.State
  ( findFailedTests
  , setAssertionTestState
  , updateTests
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.IORef (readIORef, writeIORef)

import Echidna.Types.Config (Env(..))
import Echidna.Types.Test
import Echidna.Types.Test qualified as Test

-- | Update tests based on the return value from the given function.
-- Nothing skips the update.
updateTests
  :: (MonadIO m, MonadReader Env m)
  => (EchidnaTest -> m (Maybe EchidnaTest))
  -> m ()
updateTests f = do
  testRefs <- asks (.testRefs)
  forM_ testRefs $ \testRef -> do
    test <- liftIO $ readIORef testRef
    f test >>= \case
      Just test' -> liftIO $ writeIORef testRef test'
      Nothing -> pure ()

findFailedTests
  :: (MonadIO m, MonadReader Env m)
  => m [EchidnaTest]
findFailedTests = do
  testRefs <- asks (.testRefs)
  tests <- liftIO $ traverse readIORef testRefs
  pure $ filter didFail tests

-- | Move every open assertion test for the given method signature to
-- 'newState', leaving every other test untouched.
setAssertionTestState
  :: (MonadIO m, MonadReader Env m)
  => TestState
  -> String -- ^ method signature, as returned by 'getAssertionSignature'
  -> m ()
setAssertionTestState newState signature =
  updateTests $ \test ->
    -- NOTE: the ordering matters, 'getAssertionSignature' is partial
    pure $ if isOpen test && isAssertionTest test
                          && getAssertionSignature test == signature
             then Just test { Test.state = newState }
             else Nothing
