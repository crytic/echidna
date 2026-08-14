module Echidna.Types.Campaign where

import Control.Concurrent (ThreadId)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8, Word16)
import GHC.Conc (numCapabilities)

import EVM.ABI (AbiValue(..))
import EVM.Solvers (Solver(..))

import Echidna.ABI (GenDict, emptyDict)
import Echidna.Types
import Echidna.Types.Coverage (CoverageFileType, CoverageMap)
import Echidna.Types.Signature (SolCallPrototype)
import Echidna.Types.Tx (TxResult(..))

-- | Maximum number of functions a single worker samples at once.
maxSampledFunctions :: Int
maxSampledFunctions = 10

-- | Maximum number of recent revert summaries kept per sampled function.
maxRecentReverts :: Int
maxRecentReverts = 5

-- | Per-function sampling state.
--
-- Sampling is opt-in, but once a function is sampled its stats are updated
-- once per call, on the fuzzing hot path, and only read when someone asks for
-- them. Every field is therefore kept forced: a lazy field here would retain
-- the whole call history as a chain of unevaluated updates.
data SampleStats = SampleStats
  { sampleCalls         :: !Int
    -- ^ Total calls observed.
  , sampleReverts       :: !Int
    -- ^ Total calls that did not return successfully.
  , sampleReturnRange   :: !(Maybe (AbiValue, AbiValue))
    -- ^ Min/max of the decoded return value. Only tracked for functions whose
    -- return type is known via 'Echidna.ABI.GenDict.rTypes' and orderable
    -- according to 'abiCompare'.
  , sampleRecentReverts :: ![Text]
    -- ^ Last 'maxRecentReverts' revert summaries, newest first.
  } deriving Show

emptySampleStats :: SampleStats
emptySampleStats = SampleStats 0 0 Nothing []

-- | Comparison for the subset of 'AbiValue' kinds that have a natural
-- ordering. 'Nothing' for anything else (bytes, strings, tuples, arrays), and
-- for values of different kinds.
abiCompare :: AbiValue -> AbiValue -> Maybe Ordering
abiCompare (AbiUInt _ a)  (AbiUInt _ b)  = Just (compare a b)
abiCompare (AbiInt  _ a)  (AbiInt  _ b)  = Just (compare a b)
abiCompare (AbiAddress a) (AbiAddress b) = Just (compare a b)
abiCompare (AbiBool a)    (AbiBool b)    = Just (compare a b)
abiCompare _ _ = Nothing

-- | Fold one observed call into a function's 'SampleStats'. Pure, so it can be
-- unit tested without building 'EVM.Types.VMResult' values.
applySampleEvent
  :: TxResult       -- ^ Result kind, from 'Echidna.Types.Tx.getResult'.
  -> Maybe AbiValue -- ^ Decoded return value, if available and of a known type.
  -> Text           -- ^ Function name, used in the revert summary.
  -> [AbiValue]     -- ^ Call arguments, used in the revert summary.
  -> SampleStats
  -> SampleStats
applySampleEvent result decoded fname args stats
  | result `elem` [ReturnTrue, ReturnFalse, Stop] =
      stats { sampleCalls       = stats.sampleCalls + 1
            , sampleReturnRange = maybe range (`widen` range) decoded
            }
  | otherwise =
      stats { sampleCalls         = stats.sampleCalls + 1
            , sampleReverts       = stats.sampleReverts + 1
            , sampleRecentReverts =
                takeStrict maxRecentReverts (summary : stats.sampleRecentReverts)
            }
  where
    range = stats.sampleReturnRange

    summary =
      let argStr = T.intercalate "," (map (T.pack . show) args)
      in fname <> "(" <> argStr <> "): " <> T.pack (show result)

    widen v Nothing = Just (v, v)
    widen v (Just (lo, hi)) =
      let !lo' = if abiCompare v lo == Just LT then v else lo
          !hi' = if abiCompare v hi == Just GT then v else hi
      in Just (lo', hi')

-- | Combine two per-function 'SampleStats', typically from different workers.
-- Counts are summed, ranges widened, recent reverts concatenated and capped.
mergeSampleStats :: SampleStats -> SampleStats -> SampleStats
mergeSampleStats a b = SampleStats
  { sampleCalls         = a.sampleCalls + b.sampleCalls
  , sampleReverts       = a.sampleReverts + b.sampleReverts
  , sampleReturnRange   = mergeRange a.sampleReturnRange b.sampleReturnRange
  , sampleRecentReverts =
      takeStrict maxRecentReverts (a.sampleRecentReverts ++ b.sampleRecentReverts)
  }
  where
    mergeRange Nothing x = x
    mergeRange x Nothing = x
    mergeRange (Just (loA, hiA)) (Just (loB, hiB)) =
      let !lo = if abiCompare loA loB == Just GT then loB else loA
          !hi = if abiCompare hiA hiB == Just LT then hiB else hiA
      in Just (lo, hi)

-- | 'take' that forces the spine and the elements it keeps. Plain 'take' would
-- leave the kept list holding on to the entire input through an unevaluated
-- tail, which for 'sampleRecentReverts' means the whole revert history.
takeStrict :: Int -> [a] -> [a]
takeStrict n _ | n <= 0 = []
takeStrict _ []         = []
takeStrict n (x:xs)     = x `seq` rest `seq` (x : rest)
  where rest = takeStrict (n - 1) xs

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
  , coverageExcludes   :: [Text]
    -- ^ List of glob patterns for files/directories to exclude from coverage reports
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
  , sampledFunctions :: !(Map Text SampleStats)
    -- ^ Functions whose calls are sampled for return-value range and revert
    --   history, keyed by canonical signature (e.g. @"totalSupply()"@). Empty
    --   unless sampling was explicitly enabled for this worker.
  , prioritizedSequences :: ![(Double, [SolCallPrototype])]
    -- ^ Call sequences to bias generation towards, each with the probability
    --   of being used in place of a corpus-mutated sequence. Empty unless
    --   sequences were explicitly injected into this worker.
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
              , sampledFunctions = Map.empty
              , prioritizedSequences = []
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
