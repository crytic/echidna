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
  | ClearPrioritization
  | ExecuteSequence [Tx] Bool (TMVar String)
    -- ^ Replay a concrete sequence. Reply is a JSON-encoded report.
    --   The 'Bool' flag, when 'True', additionally includes the EVM trace
    --   tree of the LAST tx in the JSON output (intermediate trees are
    --   skipped to keep cost bounded). Only worker 0 responds.
  | EnableSampling Text
    -- ^ Begin sampling the given function (canonical signature). Capped
    --   per-worker by 'maxSampledFunctions'.
  | ClearSampling
    -- ^ Drop all sampling state.

instance Show FuzzerCmd where
  show DumpLcov = "DumpLcov"
  show (SolutionFound txs) = "SolutionFound " ++ show txs
  show (FuzzSequence s p) = "FuzzSequence " ++ show s ++ " (" ++ show p ++ ")"
  show ClearPrioritization = "ClearPrioritization"
  show (ExecuteSequence txs trace _) = "ExecuteSequence " ++ show txs ++ " trace=" ++ show trace
  show (EnableSampling sig) = "EnableSampling " ++ show sig
  show ClearSampling = "ClearSampling"

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
