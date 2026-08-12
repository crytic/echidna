module Echidna.Types.Agent where

import Echidna.Types.Config (Env)
import Echidna.Types.InterWorker
import Echidna.Types.Worker (WorkerStopReason)

-- | The Agent Typeclass
class Show a => Agent a where
  getAgentId :: a -> AgentId

  -- | Run the agent to completion and report why it stopped.
  --
  -- The reason is returned rather than pushed as a 'WorkerStopped' event: an
  -- agent killed by the campaign timeout never reaches its own last line, so
  -- only the caller knows the real reason and it must emit that event itself,
  -- exactly once per worker. Listeners count 'WorkerStopped' events to decide
  -- when the campaign is over, so a second one retires a worker that is still
  -- running.
  runAgent :: a -> Bus -> Env -> IO WorkerStopReason
