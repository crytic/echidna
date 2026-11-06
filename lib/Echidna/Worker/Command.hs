-- | Commands a fuzzing worker accepts over the inter-worker bus.
module Echidna.Worker.Command (checkMessages) where

import Control.Concurrent.STM (TChan, atomically, putTMVar, tryReadTChan)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict (MonadIO, MonadState, gets, liftIO, modify')
import Data.Map qualified as Map

import EVM.Types (VM, VMType(Concrete))

import Echidna.ABI (argumentBoundaryVariants)
import Echidna.Types.Campaign
import Echidna.Types.Config (Env)
import Echidna.Types.Constraint
  (ArgumentBounds, ConstraintKey(..), maxContextsPerSignature)
import Echidna.Types.InterWorker
  (FuzzerCmd(..), Message(..), Reply(..), WrappedMessage(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Worker.Replay (executeSeq)

maxPendingConstraintProbes :: Int
maxPendingConstraintProbes = 12

-- | Run every command addressed to this worker that is currently waiting on
-- the bus, then return.
--
-- The queue is drained rather than sampled one message at a time: every
-- broadcast is delivered to every worker's duplicate, so leaving messages
-- behind would let a worker's view of the bus grow without bound while a burst
-- of coverage is being found. Commands only ever originate outside the
-- campaign, at the pace of whoever is driving it, so draining cannot starve
-- fuzzing -- though a command that runs transactions of its own does hold this
-- worker up while it does.
checkMessages
  :: (MonadIO m, MonadThrow m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete
  -- ^ The worker's initial VM, for commands that run transactions of their own
  -> TChan WrappedMessage
  -> m ()
checkMessages vm chan = do
  workerId <- gets (.workerId)
  let loop = liftIO (atomically (tryReadTChan chan)) >>= \case
        Nothing -> pure ()
        Just (WrappedMessage _ (ToFuzzer tid cmd)) | tid == workerId ->
          handleCmd vm cmd >> loop
        Just _ -> loop
  loop

handleCmd
  :: (MonadIO m, MonadThrow m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete
  -> FuzzerCmd
  -> m ()
handleCmd _ (EnableSampling sig) =
  modify' $ \workerState ->
    if Map.size workerState.sampledFunctions >= maxSampledFunctions
       || Map.member sig workerState.sampledFunctions
    then workerState
    else workerState
      { sampledFunctions =
          Map.insert sig emptySampleStats workerState.sampledFunctions
      }

handleCmd _ ClearSampling =
  modify' $ \workerState -> workerState { sampledFunctions = Map.empty }

handleCmd _ (FuzzSequence prototypes prob) =
  modify' $ \workerState -> workerState
    { prioritizedSequences = (prob, prototypes) : workerState.prioritizedSequences
    }

handleCmd _ ClearPrioritization =
  modify' $ \workerState -> workerState { prioritizedSequences = [] }

handleCmd _ (SetArgumentBounds key bounds) =
  modify' $ \workerState -> workerState
    { constraintBounds = if Map.null bounds
        then workerState.constraintBounds
        else rememberBounds key bounds workerState.constraintBounds
    }

handleCmd _ (ProbeArgumentBounds key targetTx bounds) =
  modify' $ \workerState -> workerState
    { constraintProbes = take maxPendingConstraintProbes $
        workerState.constraintProbes <> boundaryProbes
    }
  where
    boundaryProbes = case targetTx.call of
      SolCall (name, arguments) ->
        [ key.prefix <> [targetTx { call = SolCall (name, arguments') }]
        | arguments' <- argumentBoundaryVariants bounds arguments
        ]
      _ -> []

-- The command is addressed to a single worker, so this replies exactly once and
-- 'putTMVar' cannot block. The replay runs on the worker's own thread: the
-- caller waits for it, and this worker stops fuzzing until it is done.
handleCmd vm (ExecuteSequence txs includeTrace (Reply replyVar)) = do
  report <- executeSeq includeTrace vm txs
  liftIO $ atomically $ putTMVar replyVar report

-- Retain a bounded rolling sample. Exact state prefixes are intentionally not
-- hashed, so keeping this map small also bounds structural comparison costs.
rememberBounds
  :: ConstraintKey
  -> ArgumentBounds
  -> Map.Map ConstraintKey ArgumentBounds
  -> Map.Map ConstraintKey ArgumentBounds
rememberBounds key bounds cache
  | Map.member key cache = Map.insert key bounds cache
  | otherwise = Map.insert key bounds trimmed
  where
    sameSignature = Map.filterWithKey
      (\cachedKey _ -> cachedKey.signature == key.signature)
      cache
    trimmed
      | Map.size sameSignature < maxContextsPerSignature = cache
      | otherwise = case Map.lookupMin sameSignature of
          Just (previous, _) -> Map.delete previous cache
          Nothing -> cache
