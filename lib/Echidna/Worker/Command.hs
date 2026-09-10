-- | Commands a fuzzing worker accepts over the inter-worker bus.
module Echidna.Worker.Command (checkMessages) where

import Control.Concurrent.STM (TChan, atomically, tryReadTChan)
import Control.Monad.State.Strict (MonadIO, MonadState, gets, liftIO, modify')
import Data.Map qualified as Map

import Echidna.Types.Campaign
import Echidna.Types.InterWorker (FuzzerCmd(..), Message(..), WrappedMessage(..))

-- | Run every command addressed to this worker that is currently waiting on
-- the bus, then return.
--
-- The queue is drained rather than sampled one message at a time: every
-- broadcast is delivered to every worker's duplicate, so leaving messages
-- behind would let a worker's view of the bus grow without bound while a burst
-- of coverage is being found. Commands only ever originate outside the
-- campaign, at the pace of whoever is driving it, so draining cannot starve
-- fuzzing.
checkMessages
  :: (MonadIO m, MonadState WorkerState m)
  => TChan WrappedMessage
  -> m ()
checkMessages chan = do
  workerId <- gets (.workerId)
  let loop = liftIO (atomically (tryReadTChan chan)) >>= \case
        Nothing -> pure ()
        Just (WrappedMessage _ (ToFuzzer tid cmd)) | tid == workerId ->
          handleCmd cmd >> loop
        Just _ -> loop
  loop

handleCmd :: MonadState WorkerState m => FuzzerCmd -> m ()
handleCmd (EnableSampling sig) =
  modify' $ \workerState ->
    if Map.size workerState.sampledFunctions >= maxSampledFunctions
       || Map.member sig workerState.sampledFunctions
    then workerState
    else workerState
      { sampledFunctions =
          Map.insert sig emptySampleStats workerState.sampledFunctions
      }

handleCmd ClearSampling =
  modify' $ \workerState -> workerState { sampledFunctions = Map.empty }

handleCmd (FuzzSequence prototypes prob) =
  modify' $ \workerState -> workerState
    { prioritizedSequences = (prob, prototypes) : workerState.prioritizedSequences
    }

handleCmd ClearPrioritization =
  modify' $ \workerState -> workerState { prioritizedSequences = [] }
