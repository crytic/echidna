module Echidna.Types.Corpus where

import Data.List (scanl')
import Data.Set (Set, size, toList)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import Echidna.Types.Tx (Tx)

type Corpus = Set (Int, [Tx])

corpusSize :: Corpus -> Int
corpusSize = size

-- | Snapshot of the corpus prepared for weighted random selection: the
-- sequences in one vector, the cumulative sums of their weights in another.
-- Building it costs one corpus traversal, after which each draw is a binary
-- search (see 'Echidna.Mutator.Corpus.selectFromCorpus') instead of
-- re-listing and re-summing the whole Set. Only rebuilt when the corpus
-- grows; see 'corpusSelector' in 'Echidna.Types.Campaign.WorkerState'.
data CorpusSelector = CorpusSelector
  { seqs       :: !(V.Vector [Tx])
  , cumWeights :: !(VU.Vector Int)
    -- ^ inclusive prefix sums of the sequence weights; the last entry is the
    -- total weight. Weights are the 'ncallseqs' stamps given on insertion,
    -- so younger sequences are favored.
  }

mkCorpusSelector :: Corpus -> CorpusSelector
mkCorpusSelector corpus = CorpusSelector
  { seqs = V.fromListN n (map snd entries)
  , cumWeights = VU.fromListN n (drop 1 $ scanl' (+) 0 (map fst entries))
  }
  where
    n = size corpus
    entries = toList corpus
