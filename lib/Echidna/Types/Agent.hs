module Echidna.Types.Agent where

import Data.IORef (IORef)
import Data.Text (Text)

import EVM.Types (VM, VMType(Concrete))

import Echidna.ABI (GenDict)
import Echidna.Types.Campaign (WorkerState)
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker (WorkerId, WorkerType(..))

-- | A worker the campaign can spawn, bundled with everything it needs to run.
--
-- Each constructor carries only its own worker's parameters, so a caller never
-- has to supply arguments the other kind of worker discards.
data Agent
  = FuzzerAgent
      { fuzzerId :: Int
      , initialVm :: VM Concrete
      , initialDict :: GenDict
      , initialCorpus :: [(FilePath, [Tx])]
      , testLimit :: Int
      , stateRef :: IORef WorkerState
      }
  -- | The symbolic worker does not replay the corpus, so it carries none.
  | SymbolicAgent
      { initialVm :: VM Concrete
      , initialDict :: GenDict
      , contractName :: Maybe Text
      , stateRef :: IORef WorkerState
      }

-- | Construct a fuzzing agent with an explicit state publication reference.
--
-- Keeping the reference as an argument lets timeout-triggered auxiliary passes
-- use a throwaway reference instead of overwriting the reporting state.
mkFuzzerAgent
  :: VM Concrete
  -> GenDict
  -> Int
  -> IORef WorkerState
  -> [(FilePath, [Tx])]
  -> Int
  -> Agent
mkFuzzerAgent vm dict workerId stateRef initialCorpus testLimit =
  FuzzerAgent { fuzzerId = workerId
              , initialVm = vm
              , initialDict = dict
              , initialCorpus
              , testLimit
              , stateRef
              }

-- | The worker id this agent runs as. There is at most one symbolic worker and
-- it is always worker 0.
workerIdOf :: Agent -> WorkerId
workerIdOf FuzzerAgent{fuzzerId} = fuzzerId
workerIdOf SymbolicAgent{} = 0

-- | The kind of worker this agent is, for tagging campaign events.
workerTypeOf :: Agent -> WorkerType
workerTypeOf FuzzerAgent{} = FuzzWorker
workerTypeOf SymbolicAgent{} = SymbolicWorker

-- | The ref the agent publishes its 'WorkerState' through.
stateRefOf :: Agent -> IORef WorkerState
stateRefOf FuzzerAgent{stateRef} = stateRef
stateRefOf SymbolicAgent{stateRef} = stateRef

-- | Name used to announce the agent when it starts.
agentName :: Agent -> String
agentName FuzzerAgent{} = "FuzzerAgent"
agentName SymbolicAgent{} = "SymbolicAgent"
