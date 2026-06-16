module Echidna.Types.Agent where

import Echidna.Types.Config (Env)
import Echidna.Types.InterWorker

-- | The Agent Typeclass
class Show a => Agent a where
  getAgentId :: a -> AgentId
  runAgent :: a -> Bus -> Env -> IO ()
