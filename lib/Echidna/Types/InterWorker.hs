module Echidna.Types.InterWorker where

import Control.Concurrent.STM
import Data.Text (Text)

import EVM.ABI (AbiValue)
import Echidna.Types.Tx (Tx)
import Echidna.Types.Test (EchidnaTest)

-- | Agent Identities
data AgentId = FuzzerId Int | SymbolicId | AIId
  deriving (Show, Eq, Ord)

-- | Fuzzer specific commands
data FuzzerCmd
  = DumpLcov
  | SolutionFound [Tx]
  | FuzzSequence [(Text, [Maybe AbiValue])] Double
    -- ^ Prioritize a sequence of function calls (upstream version)
  | ClearPrioritization
  | ExecuteSequence [Tx] (Maybe (TMVar Bool))
  | InjectTransaction Tx
    -- ^ Inject a single transaction for execution (MCP control, US2)
  | LogMCPCommand Text [(Text, Text)]
    -- ^ Log an MCP command execution for reproducibility (FR-010)

instance Show FuzzerCmd where
  show DumpLcov = "DumpLcov"
  show (SolutionFound txs) = "SolutionFound " ++ show txs
  show (FuzzSequence s p) = "FuzzSequence " ++ show s ++ " (" ++ show p ++ ")"
  show ClearPrioritization = "ClearPrioritization"
  show (ExecuteSequence txs _) = "ExecuteSequence " ++ show txs
  show (InjectTransaction tx) = "InjectTransaction " ++ show tx
  show (LogMCPCommand tool params) = "LogMCPCommand " ++ show tool

-- | Symbolic specific commands
newtype SymbolicCmd
  = SolveThis [Tx]
  deriving (Show)

-- | Message Protocol
data Message
  = Broadcast BroadcastMsg
  | ToFuzzer Int FuzzerCmd
  | ToSymbolic SymbolicCmd
  | Request RequestMsg
  deriving (Show)

data BroadcastMsg
  = NewCoverageInfo Int [Tx] Bool -- points, transactions, isReplaying
  | FoundBug EchidnaTest
  | StrategyUpdate Text
  | WorkerStopped AgentId
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
