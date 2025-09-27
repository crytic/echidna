module Echidna.Types.Campaign where

import Control.Concurrent (ThreadId)
import Data.Text (Text)
import Data.Word (Word8, Word16)
import GHC.Conc (numCapabilities)

import EVM.Solvers (Solver(..))

import Echidna.ABI (GenDict, emptyDict)
import Echidna.Types
import Echidna.Types.Coverage (CoverageFileType, CoverageMap)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf
  { testLimit          :: Int
    -- ^ Maximum number of function calls to execute while fuzzing
  , stopOnFail         :: Bool
    -- ^ Whether to stop the campaign immediately if any property fails
  , seqLen             :: Int
    -- ^ Number of calls between state resets (e.g. \"every 10 calls,
    -- reset the state to avoid unrecoverable states/save memory\"
  , shrinkLimit        :: Int
    -- ^ Maximum number of candidate sequences to evaluate while shrinking
  , knownCoverage      :: Maybe CoverageMap
    -- ^ If applicable, initially known coverage. If this is 'Nothing',
    -- Echidna won't collect coverage information (and will go faster)
  , seed               :: Maybe Int
    -- ^ Seed used for the generation of random transactions
  , dictFreq           :: Float
    -- ^ Frequency for the use of dictionary values in the random transactions
  , corpusDir          :: Maybe FilePath
    -- ^ Directory to load and save lists of transactions
  , coverageDir        :: Maybe FilePath
    -- ^ Directory to save coverage reports
  , mutConsts          :: MutationConsts Integer
    -- ^ Mutation constants for fuzzing
  , coverageFormats    :: [CoverageFileType]
    -- ^ List of file formats to save coverage reports
  , workers            :: Maybe Word8
    -- ^ Number of fuzzing workers
  , serverPort         :: Maybe Word16
    -- ^ Server-Sent Events HTTP port number, if missing server is not ran
  , symExec            :: Bool
    -- ^ Whether to add an additional symbolic execution worker
  , symExecSMTSolver   :: Solver
    -- ^ SMT solver to use for symbolic execution.
    -- Supported solvers: "cvc5", "z3" and "bitwuzla"
  , symExecTargets     :: [Text]
    -- ^ List of target functions for symbolic execution.
    -- If this is empty, all functions are considered targets.
  , symExecTimeout     :: Int
    -- ^ Timeout for symbolic execution SMT solver queries.
    -- Only relevant if symExec is True
  , symExecNSolvers    :: Int
    -- ^ Number of SMT solvers used in symbolic execution.
    -- Only relevant if symExec is True
  , symExecMaxIters    :: Integer
    -- ^ Number of times we may revisit a particular branching point.
    -- Only relevant if symExec is True
  , symExecAskSMTIters :: Integer
    -- ^ Number of times we may revisit a particular branching point
    -- before we consult the SMT solver to check reachability.
    -- Only relevant if symExec is True
  , symExecMaxExplore :: Integer
    -- ^ Maximum number of states to explore before we stop exploring it.
    -- Only relevant if symExec is True
  }

-- | The state of a fuzzing campaign.
data WorkerState = WorkerState
  { workerId    :: !Int
    -- ^ Worker ID starting from 0
  , genDict     :: !GenDict
    -- ^ Generation dictionary
  , newCoverage :: !Bool
    -- ^ Flag to indicate new coverage found
  , ncallseqs   :: !Int
    -- ^ Number of times the callseq is called
  , ncalls      :: !Int
    -- ^ Number of calls executed while fuzzing
  , totalGas    :: !Int
    -- ^ Total gas consumed while fuzzing
  , runningThreads :: [ThreadId]
    -- ^ Extra threads currently being run,
    --   aside from the main worker thread
  }

initialWorkerState :: WorkerState
initialWorkerState =
  WorkerState { workerId = 0
              , genDict = emptyDict
              , newCoverage = False
              , ncallseqs = 0
              , ncalls = 0
              , totalGas = 0
              , runningThreads = []
              }

defaultTestLimit :: Int
defaultTestLimit = 50000

defaultSequenceLength :: Int
defaultSequenceLength = 100

defaultShrinkLimit :: Int
defaultShrinkLimit = 5000

defaultSymExecTimeout :: Int
defaultSymExecTimeout = 30

defaultSymExecNWorkers :: Int
defaultSymExecNWorkers = 1

defaultSymExecMaxExplore :: Integer
defaultSymExecMaxExplore = 10

defaultSymExecMaxIters :: Integer
defaultSymExecMaxIters = 5

-- | Same default as in hevm, "everything else is unsound"
-- (https://github.com/argotorg/hevm/pull/252)
defaultSymExecAskSMTIters :: Integer
defaultSymExecAskSMTIters = 1

-- | Get number of fuzzing workers (doesn't include sym exec worker)
-- Defaults to `N` if set to Nothing, where `N` is Haskell's -N value,
-- usually the number of cores, clamped between 1 and 4.
getNFuzzWorkers :: CampaignConf -> Int
getNFuzzWorkers conf = maybe defaultN fromIntegral conf.workers
  where
    n = numCapabilities
    maxN = max 1 n
    defaultN = min 4 maxN -- capped at 4 by default
