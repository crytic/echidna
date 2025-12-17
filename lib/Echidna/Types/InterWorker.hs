module Echidna.Types.InterWorker where

import Control.Concurrent.STM
import Data.Text (Text)

import Echidna.Types.Tx (Tx)
import Echidna.Types.Test (EchidnaTest)

-- | Agent Identities
data AgentId = FuzzerId Int | SymbolicId | AIId
  deriving (Show, Eq, Ord)

-- | Message Protocol
data Message
  = Broadcast BroadcastMsg
  | Direct AgentId DirectMsg
  | Request RequestMsg
  deriving (Show)

data BroadcastMsg
  = NewCoverageInfo Int [Tx] -- points, transactions
  | FoundBug EchidnaTest
  | StrategyUpdate Text
  | WorkerStopped AgentId
  deriving (Show)

data DirectMsg
  = SolveThis [Tx]
  | SolutionFound [Tx]
  deriving (Show)

data RequestMsg
  = HelpMe
  deriving (Show)

-- | Message Envelope
data WrappedMessage = WrappedMessage
  { from :: AgentId
  , content :: Message
  } deriving (Show)

-- | Shared Communication Bus
type Bus = TChan WrappedMessage
