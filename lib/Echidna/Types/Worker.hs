module Echidna.Types.Worker where

import Echidna.Types.Test
import Echidna.Types.Tx

data WorkerType = FuzzWorker | SymbolicWorker deriving (Eq)

type WorkerId = Int

data CampaignEvent
  = WorkerEvent WorkerId WorkerType WorkerEvent
  | Failure String
  | ReproducerSaved String -- filename

data WorkerEvent
  = TestFalsified !EchidnaTest
  | TestOptimized !EchidnaTest
  | NewCoverage { points :: !Int, numCodehashes :: !Int, corpusSize :: !Int, transactions :: [Tx] }
  | SymExecError !String
  | SymExecLog !String
  | TxSequenceReplayed FilePath !Int !Int
  | TxSequenceReplayFailed FilePath Tx
  | WorkerStopped WorkerStopReason
  -- ^ This is a terminal event. Worker exits and won't push any events after
  -- this one
  deriving Show

data WorkerStopReason
  = TestLimitReached
  | SymbolicExplorationDone
  | SymbolicVerificationDone
  | TimeLimitReached
  | FastFailed
  | Killed !String
  | Crashed !String
  deriving Show

