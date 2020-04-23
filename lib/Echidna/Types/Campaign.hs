{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Types.Campaign where

import Control.Lens
import Data.Aeson (ToJSON(..), object)
import Data.Foldable (toList)
import Data.Has (Has(..))
import Data.Map (Map, mapKeys)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Text (Text)
import EVM.Keccak (keccak)
import Numeric (showHex)

import Echidna.ABI (GenDict, defaultDict)
import Echidna.Exec (CoverageMap, ExecException)
import Echidna.Solidity (SolTest)
import Echidna.Types.Tx (Tx)

type MutationConsts = (Integer, Integer, Integer)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf { _testLimit     :: Int
                                   -- ^ Maximum number of function calls to execute while fuzzing
                                 , _stopOnFail    :: Bool
                                   -- ^ Whether to stop the campaign immediately if any property fails
                                 , _estimateGas   :: Bool
                                   -- ^ Whether to collect gas usage statistics
                                 , _seqLen        :: Int
                                   -- ^ Number of calls between state resets (e.g. \"every 10 calls,
                                   -- reset the state to avoid unrecoverable states/save memory\"
                                 , _shrinkLimit   :: Int
                                   -- ^ Maximum number of candidate sequences to evaluate while shrinking
                                 , _knownCoverage :: Maybe CoverageMap
                                   -- ^ If applicable, initially known coverage. If this is 'Nothing',
                                   -- Echidna won't collect coverage information (and will go faster)
                                 , _seed          :: Maybe Int
                                   -- ^ Seed used for the generation of random transactions
                                 , _dictFreq      :: Float
                                   -- ^ Frequency for the use of dictionary values in the random transactions
                                 , _corpusDir     :: Maybe FilePath
                                   -- ^ Directory to load and save lists of transactions
                                 , _mutConsts     :: MutationConsts
                                 }
makeLenses ''CampaignConf

-- | State of a particular Echidna test. N.B.: \"Solved\" means a falsifying call sequence was found.
data TestState = Open Int             -- ^ Maybe solvable, tracking attempts already made
               | Large Int [Tx]       -- ^ Solved, maybe shrinable, tracking shrinks tried + best solve
               | Passed               -- ^ Presumed unsolvable
               | Solved [Tx]          -- ^ Solved with no need for shrinking
               | Failed ExecException -- ^ Broke the execution environment
                 deriving Show

instance Eq TestState where
  (Open i)    == (Open j)    = i == j
  (Large i l) == (Large j m) = i == j && l == m
  Passed      == Passed      = True
  (Solved l)  == (Solved m)  = l == m
  _           == _           = False

instance ToJSON TestState where
  toJSON s = object $ ("passed", toJSON passed) : maybeToList desc where
    (passed, desc) = case s of Open _    -> (True, Nothing)
                               Passed    -> (True, Nothing)
                               Large _ l -> (False, Just ("callseq", toJSON l))
                               Solved  l -> (False, Just ("callseq", toJSON l))
                               Failed  e -> (False, Just ("exception", toJSON $ show e))

type Corpus = Set (Integer, [Tx])

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests       :: [(SolTest, TestState)]
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
                         }
makeLenses ''Campaign

instance ToJSON Campaign where
  toJSON (Campaign ts co gi _ _ _ _) = object $ ("tests", toJSON $ mapMaybe format ts)
    : ((if co == mempty then [] else [
    ("coverage",) . toJSON . mapKeys (("0x" <>) . (`showHex` "") . keccak) $ toList <$> co]) ++
       [(("maxgas",) . toJSON . toList) gi | gi /= mempty]) where
        format (Right _,      Open _) = Nothing
        format (Right (n, _), s)      = Just ("assertion in " <> n, toJSON s)
        format (Left (n, _),  s)      = Just (n,                    toJSON s)

instance Has GenDict Campaign where
  hasLens = genDict

defaultCampaign :: Campaign
defaultCampaign = Campaign mempty mempty mempty defaultDict False mempty 0
