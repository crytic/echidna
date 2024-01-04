module Echidna.Types.Campaign where

import Data.Aeson
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8, Word16)

import Echidna.ABI (GenDict, emptyDict, encodeSig)
import Echidna.Output.Source (CoverageFileType)
import Echidna.Types
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Test (TestType (..), EchidnaTest(..))
import Echidna.Types.Tx (Tx)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf
  { testLimit       :: Int
    -- ^ Maximum number of function calls to execute while fuzzing
  , stopOnFail      :: Bool
    -- ^ Whether to stop the campaign immediately if any property fails
  , estimateGas     :: Bool
    -- ^ Whether to collect gas usage statistics
  , seqLen          :: Int
    -- ^ Number of calls between state resets (e.g. \"every 10 calls,
    -- reset the state to avoid unrecoverable states/save memory\"
  , shrinkLimit     :: Int
    -- ^ Maximum number of candidate sequences to evaluate while shrinking
  , knownCoverage   :: Maybe CoverageMap
    -- ^ If applicable, initially known coverage. If this is 'Nothing',
    -- Echidna won't collect coverage information (and will go faster)
  , seed            :: Maybe Int
    -- ^ Seed used for the generation of random transactions
  , dictFreq        :: Float
    -- ^ Frequency for the use of dictionary values in the random transactions
  , corpusDir       :: Maybe FilePath
    -- ^ Directory to load and save lists of transactions
  , mutConsts       :: MutationConsts Integer
    -- ^ Directory to load and save lists of transactions
  , coverageFormats :: [CoverageFileType]
    -- ^ List of file formats to save coverage reports
  , workers         :: Maybe Word8
    -- ^ Number of fuzzing workers
  , serverPort      :: Maybe Word16
    -- ^ Server-Sent Events HTTP port number, if missing server is not ran
  }

data CampaignEvent
  = TestFalsified !EchidnaTest
  | TestOptimized !EchidnaTest
  | NewCoverage !Int !Int !Int
  | TxSequenceReplayed !Int !Int
  | WorkerStopped WorkerStopReason
  -- ^ This is a terminal event. Worker exits and won't push any events after
  -- this one
  deriving Show

instance ToJSON CampaignEvent where
  toJSON = \case
    TestFalsified test -> toJSON test
    TestOptimized test -> toJSON test
    NewCoverage coverage numContracts corpusSize ->
      object [ "coverage" .= coverage, "contracts" .= numContracts, "corpus_size" .= corpusSize]
    TxSequenceReplayed current total -> object [ "current" .= current, "total" .= total ]
    WorkerStopped reason -> object [ "reason" .= show reason ]

data WorkerStopReason
  = TestLimitReached
  | TimeLimitReached
  | FastFailed
  | Killed !String
  | Crashed !String
  deriving Show

ppCampaignEvent :: CampaignEvent -> String
ppCampaignEvent = \case
  TestFalsified test ->
    let name = case test.testType of
                 PropertyTest n _ -> n
                 AssertionTest _ n _ -> encodeSig n
                 CallTest n _ -> n
                 _ -> error "impossible"
    in "Test " <> T.unpack name <> " falsified!"
  TestOptimized test ->
    let name = case test.testType of OptimizationTest n _ -> n; _ -> error "fixme"
    in "New maximum value of " <> T.unpack name <> ": " <> show test.value
  NewCoverage points codehashes corpus ->
    "New coverage: " <> show points <> " instr, "
      <> show codehashes <> " contracts, "
      <> show corpus <> " seqs in corpus"
  TxSequenceReplayed current total ->
    "Sequence replayed from corpus (" <> show current <> "/" <> show total <> ")"
  WorkerStopped TestLimitReached ->
    "Test limit reached. Stopping."
  WorkerStopped TimeLimitReached ->
    "Time limit reached. Stopping."
  WorkerStopped FastFailed ->
    "A test was falsified. Stopping."
  WorkerStopped (Killed e) ->
    "Killed (" <> e <>"). Stopping."
  WorkerStopped (Crashed e) ->
    "Crashed:\n\n" <>
    e <>
    "\n\nPlease report it to https://github.com/crytic/echidna/issues"

-- | The state of a fuzzing campaign.
data WorkerState = WorkerState
  { workerId    :: !Int
    -- ^ Worker ID starting from 0
  , gasInfo     :: !(Map Text (Gas, [Tx]))
    -- ^ Worst case gas (NOTE: we don't always record this)
  , genDict     :: !GenDict
    -- ^ Generation dictionary
  , newCoverage :: !Bool
    -- ^ Flag to indicate new coverage found
  , ncallseqs   :: !Int
    -- ^ Number of times the callseq is called
  , ncalls      :: !Int
    -- ^ Number of calls executed while fuzzing
  }

initialWorkerState :: WorkerState
initialWorkerState =
  WorkerState { workerId = 0
              , gasInfo = mempty
              , genDict = emptyDict
              , newCoverage = False
              , ncallseqs = 0
              , ncalls = 0
              }

defaultTestLimit :: Int
defaultTestLimit = 50000

defaultSequenceLength :: Int
defaultSequenceLength = 100

defaultShrinkLimit :: Int
defaultShrinkLimit = 5000
