module Echidna.Types.Campaign where

import Data.Map (Map)
import Data.Text (Text)

import Echidna.ABI (GenDict, emptyDict)
import Echidna.Types
import Echidna.Types.Corpus
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Test (EchidnaTest)
import Echidna.Types.Tx (Tx)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf
  { testLimit     :: Int
    -- ^ Maximum number of function calls to execute while fuzzing
  , stopOnFail    :: Bool
    -- ^ Whether to stop the campaign immediately if any property fails
  , estimateGas   :: Bool
    -- ^ Whether to collect gas usage statistics
  , seqLen        :: Int
    -- ^ Number of calls between state resets (e.g. \"every 10 calls,
    -- reset the state to avoid unrecoverable states/save memory\"
  , shrinkLimit   :: Int
    -- ^ Maximum number of candidate sequences to evaluate while shrinking
  , knownCoverage :: Maybe CoverageMap
    -- ^ If applicable, initially known coverage. If this is 'Nothing',
    -- Echidna won't collect coverage information (and will go faster)
  , seed          :: Maybe Int
    -- ^ Seed used for the generation of random transactions
  , dictFreq      :: Float
    -- ^ Frequency for the use of dictionary values in the random transactions
  , corpusDir     :: Maybe FilePath
    -- ^ Directory to load and save lists of transactions
  , mutConsts     :: MutationConsts Integer
    -- ^ Directory to load and save lists of transactions
  , coverageReport :: Bool
    -- ^ Whether or not to generate a coverage report
  }

-- | The state of a fuzzing campaign.
data Campaign = Campaign
  { tests       :: ![EchidnaTest]
    -- ^ Tests being evaluated
  , coverage    :: !CoverageMap
    -- ^ Coverage captured (NOTE: we don't always record this)
  , gasInfo     :: !(Map Text (Gas, [Tx]))
    -- ^ Worst case gas (NOTE: we don't always record this)
  , genDict     :: !GenDict
    -- ^ Generation dictionary
  , newCoverage :: !Bool
    -- ^ Flag to indicate new coverage found
  , corpus      :: !Corpus
    -- ^ List of transactions with maximum coverage
  , ncallseqs   :: !Int
    -- ^ Number of times the callseq is called
  }

defaultCampaign :: Campaign
defaultCampaign = Campaign mempty mempty mempty emptyDict False mempty 0

defaultTestLimit :: Int
defaultTestLimit = 50000

defaultSequenceLength :: Int
defaultSequenceLength = 100

defaultShrinkLimit :: Int
defaultShrinkLimit = 5000
