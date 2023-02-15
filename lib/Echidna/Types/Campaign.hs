{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Campaign where

import Control.Lens
import Data.Map (Map)
import Data.Text (Text)

import Echidna.ABI (GenDict, defaultDict)
import Echidna.Types
import Echidna.Types.Corpus
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Test (EchidnaTest)
import Echidna.Types.Signature (BytecodeMemo)
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
  }

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests       :: [EchidnaTest]
                           -- ^ Tests being evaluated
                         , _coverage    :: CoverageMap
                           -- ^ Coverage captured (NOTE: we don't always record this)
                         , _gasInfo     :: Map Text (Int, [Tx])
                           -- ^ Worst case gas (NOTE: we don't always record this)
                         , _genDict     :: GenDict
                           -- ^ Generation dictionary
                         , _newCoverage :: Bool
                           -- ^ Flag to indicate new coverage found
                         , _corpus      :: Corpus
                           -- ^ List of transactions with maximum coverage
                         , _ncallseqs   :: Int
                           -- ^ Number of times the callseq is called
                         , _bcMemo        :: BytecodeMemo
                           -- ^ Stored results of getBytecodeMetadata on all contracts
                         }
makeLenses ''Campaign

defaultCampaign :: Campaign
defaultCampaign = Campaign mempty mempty mempty defaultDict False mempty 0 mempty

defaultTestLimit :: Int
defaultTestLimit = 50000

defaultSequenceLength :: Int
defaultSequenceLength = 100

defaultShrinkLimit :: Int
defaultShrinkLimit = 5000
