module Echidna.Types.Campaign where

import Control.Concurrent (ThreadId)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word16)
import GHC.Conc (numCapabilities)

import EVM.Solvers (Solver(..))
import EVM.ABI (AbiValue(..))

import Echidna.ABI (GenDict, emptyDict)
import Echidna.Types
import Echidna.Types.Coverage (CoverageFileType, CoverageMap)
import Echidna.Types.Tx (TxResult(..))

-- | Maximum number of functions that can be sampled simultaneously.
maxSampledFunctions :: Int
maxSampledFunctions = 10

-- | Maximum number of recent revert entries kept per sampled function.
maxRecentReverts :: Int
maxRecentReverts = 5

-- | Per-function sampling state. Updated on the fuzzer hot path so all
-- fields must be strict (see GA's MCP postmortem on thunk retention).
data SampleStats = SampleStats
  { sampleCalls         :: !Int
    -- ^ Total calls observed.
  , sampleReverts       :: !Int
    -- ^ Total calls that did not return successfully.
  , sampleReturnRange   :: !(Maybe (AbiValue, AbiValue))
    -- ^ Min/max of the decoded return value (only tracked for functions
    -- with a single numeric return type known via 'GenDict.rTypes').
  , sampleRecentReverts :: ![Text]
    -- ^ Last 'maxRecentReverts' revert summaries, newest first.
  } deriving Show

emptySampleStats :: SampleStats
emptySampleStats = SampleStats 0 0 Nothing []

-- | Total comparison for the subset of 'AbiValue' kinds that have a natural
-- ordering. Returns 'Nothing' for anything else (e.g. bytes, strings, tuples,
-- arrays).
abiCompare :: AbiValue -> AbiValue -> Maybe Ordering
abiCompare (AbiUInt _ a)  (AbiUInt _ b)  = Just (compare a b)
abiCompare (AbiInt  _ a)  (AbiInt  _ b)  = Just (compare a b)
abiCompare (AbiAddress a) (AbiAddress b) = Just (compare a b)
abiCompare (AbiBool a)    (AbiBool b)    = Just (compare a b)
abiCompare _ _ = Nothing

-- | Apply one observed call (its result kind, possibly decoded return value,
-- function name, and args) to an existing 'SampleStats' record. Pure so
-- it can be unit tested without constructing 'VMResult' values.
applySampleEvent
  :: TxResult       -- ^ Result kind from 'getResult'.
  -> Maybe AbiValue -- ^ Decoded return value, if available and a known type.
  -> Text           -- ^ Function name (used in revert summary).
  -> [AbiValue]     -- ^ Call args (used in revert summary).
  -> SampleStats
  -> SampleStats
applySampleEvent result decoded fname args stats =
  let isSuccess = result `elem` [ReturnTrue, ReturnFalse, Stop]
      !range' = case decoded of
        Just v  -> updateRange v stats.sampleReturnRange
        Nothing -> stats.sampleReturnRange
  in if isSuccess
     then stats { sampleCalls       = stats.sampleCalls + 1
                , sampleReturnRange = range'
                }
     else
       let !summary = revertSummary fname args result
       in stats { sampleCalls         = stats.sampleCalls + 1
                , sampleReverts       = stats.sampleReverts + 1
                , sampleRecentReverts = take maxRecentReverts
                                          (summary : stats.sampleRecentReverts)
                }
  where
    revertSummary fname' args' r =
      let argStr = T.intercalate "," (map (T.pack . show) args')
      in fname' <> "(" <> argStr <> "): " <> T.pack (show r)

    updateRange v Nothing = Just (v, v)
    updateRange v (Just (lo, hi)) =
      let lo' = case abiCompare v lo of Just Prelude.LT -> v; _ -> lo
          hi' = case abiCompare v hi of Just Prelude.GT -> v; _ -> hi
      in Just (lo', hi')

-- | Combine two per-function 'SampleStats' (typically from different
-- workers). Counts are summed, ranges are widened, recent reverts are
-- concatenated and capped at 'maxRecentReverts'.
mergeSampleStats :: SampleStats -> SampleStats -> SampleStats
mergeSampleStats a b = SampleStats
  { sampleCalls         = a.sampleCalls + b.sampleCalls
  , sampleReverts       = a.sampleReverts + b.sampleReverts
  , sampleReturnRange   = mergeRange a.sampleReturnRange b.sampleReturnRange
  , sampleRecentReverts = take maxRecentReverts
                            (a.sampleRecentReverts ++ b.sampleRecentReverts)
  }
  where
    mergeRange Nothing x = x
    mergeRange x Nothing = x
    mergeRange (Just (loA, hiA)) (Just (loB, hiB)) =
      let lo = case abiCompare loA loB of Just Prelude.GT -> loB; _ -> loA
          hi = case abiCompare hiA hiB of Just Prelude.LT -> hiB; _ -> hiA
      in Just (lo, hi)

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
  , prioritizedSequences :: ![(Double, [(Text, [Maybe AbiValue])])]
    -- ^ Sequences of functions to prioritize during fuzzing
  , sampledFunctions :: !(Map Text SampleStats)
    -- ^ Functions whose calls are sampled for min/max return values and
    --   revert history. Keyed by canonical signature (e.g. "totalSupply()").
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
              , prioritizedSequences = []
              , sampledFunctions = Map.empty
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
