module Echidna.Agent (runAgent) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (get)
import Control.Monad.Trans (liftIO)
import Data.IORef (writeIORef)

import Echidna.Types.Agent
import Echidna.Types.Config (Env)
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..), WorkerStopReason)
import Echidna.Worker (pushCampaignEvent)
import Echidna.Worker.Fuzz (runFuzzWorker)
import Echidna.Worker.Symbolic (runSymWorker)

-- | Run an agent to completion and report why it stopped.
--
-- The reason is returned rather than pushed as a 'WorkerStopped' event: an
-- agent killed by the campaign timeout never reaches its own last line, so only
-- the caller knows the real reason and it must emit that event itself, exactly
-- once per worker. Listeners count 'WorkerStopped' events to decide when the
-- campaign is over, so a second one retires a worker that is still running.
runAgent :: Agent -> Env -> IO WorkerStopReason
runAgent agent env = do
  let workerId = workerIdOf agent
      stateRef = stateRefOf agent
      -- Publish the worker state so the UI can read it
      callback = get >>= liftIO . writeIORef stateRef

  pushCampaignEvent env $ WorkerEvent workerId (workerTypeOf agent)
    (Log ("Starting " ++ agentName agent ++ " " ++ show workerId))

  (reason, finalState) <- flip runReaderT env $ case agent of
    FuzzerAgent{initialVm, initialDict, initialCorpus, testLimit} ->
      runFuzzWorker callback initialVm initialDict workerId initialCorpus testLimit
    SymbolicAgent{initialVm, initialDict, contractName} ->
      runSymWorker callback initialVm initialDict workerId contractName

  -- The callback publishes as the worker goes, but not from every exit path
  -- (verification mode never runs it), so publish the final state here too.
  writeIORef stateRef finalState
  pure reason
