module Echidna.Mutator.Corpus where

import Control.Monad (replicateM)
import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Data.Maybe (maybeToList)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import Echidna.Mutator.Array
import Echidna.Transaction (forceMutateTx, mutateTx, shrinkTx)
import Echidna.Types (MutationConsts)
import Echidna.Types.Corpus (CorpusSelector(..))
import Echidna.Types.Random (weighted)
import Echidna.Types.Tx (Tx)

defaultMutationConsts :: Num a => MutationConsts a
defaultMutationConsts = (1, 1, 1, 1)

data TxsMutation = Identity
                 | Shrinking
                 | Mutation
                 | Expansion
                 | Swapping
                 | Deletion
  deriving (Eq, Ord, Show)

data CorpusMutation = RandomAppend TxsMutation
                    | RandomPrepend TxsMutation
                    | RandomSplice
                    | RandomInterleave
  deriving (Eq, Ord, Show)

mutator :: MonadRandom m => TxsMutation -> [Tx] -> m [Tx]
mutator Identity  = return
mutator Shrinking = mapM shrinkTx
mutator Mutation = \case
  -- 'mutateTx' rarely changes anything, which on a single transaction is a
  -- verbatim replay. Force a change, or drop the transaction so the caller
  -- pads with a fresh one.
  [tx] -> maybeToList <$> forceMutateTx tx
  txs -> mapM mutateTx txs
mutator Expansion = expandRandList
mutator Swapping = swapRandList
mutator Deletion = deleteRandList

-- | Range of the prefix length taken from a stored sequence. Identity takes a
-- strict prefix, since replaying it unchanged finds nothing; the other
-- mutations need a non-empty one.
cutRange :: TxsMutation -> Int -> (Int, Int)
cutRange Identity len = (0, max 0 (len - 1))
cutRange _ len = (min 1 len, len)

selectAndMutate
  :: MonadRandom m
  => TxsMutation
  -> CorpusSelector
  -> m [Tx]
selectAndMutate m sel = do
  rtxs <- selectFromCorpus sel
  k <- getRandomR (cutRange m (length rtxs))
  mutator m $ take k rtxs

selectAndCombine
  :: MonadRandom m
  => ([Tx] -> [Tx] -> m [Tx])
  -> Int
  -> CorpusSelector
  -> m Tx
  -> m [Tx]
selectAndCombine f ql sel genOne = do
  rtxs1 <- selectFromCorpus sel
  rtxs2 <- selectFromCorpus sel
  txs <- take ql <$> f rtxs1 rtxs2
  gtxs <- replicateM (ql - length txs) genOne
  pure $ txs <> gtxs

-- | Pick a sequence with probability proportional to its weight: draw a point
-- in the total weight, then binary-search the cumulative weights for the
-- sequence whose slice covers it.
selectFromCorpus
  :: MonadRandom m
  => CorpusSelector
  -> m [Tx]
selectFromCorpus sel = do
  r <- getRandomR (0, VU.last sel.cumWeights - 1)
  pure $ sel.seqs V.! firstGreater r
  where
    -- smallest index whose cumulative weight exceeds r; r < total weight
    -- guarantees one exists
    firstGreater r = go 0 (VU.length sel.cumWeights - 1)
      where
        go lo hi
          | lo >= hi = lo
          | sel.cumWeights VU.! mid > r = go lo mid
          | otherwise = go (mid + 1) hi
          where mid = (lo + hi) `div` 2

-- | A corpus mutation takes the target sequence length, the prepared corpus,
-- and a generator for filler transactions, run only as many times as the
-- mutated sequence needs topping up to that length.
getCorpusMutation
  :: MonadRandom m
  => CorpusMutation
  -> (Int -> CorpusSelector -> m Tx -> m [Tx])
getCorpusMutation (RandomAppend m) = \ql sel genOne -> do
  rtxs' <- take ql <$> selectAndMutate m sel
  gtxs <- replicateM (ql - length rtxs') genOne
  pure $ rtxs' ++ gtxs
getCorpusMutation (RandomPrepend m) = \ql sel genOne -> do
  rtxs' <- selectAndMutate m sel
  k <- getRandomR (0, ql - 1)
  let mid = take (ql - k) rtxs'
  -- Pad with fresh transactions so the sequence has ql entries.
  gtxs <- replicateM (ql - length mid) genOne
  pure $ take k gtxs ++ mid ++ drop k gtxs
getCorpusMutation RandomSplice = selectAndCombine spliceAtRandom
getCorpusMutation RandomInterleave = selectAndCombine interleaveAtRandom

seqMutatorsStateful
  :: MonadRandom m
  => MutationConsts Integer
  -> m CorpusMutation
seqMutatorsStateful (c1, c2, c3, c4) = weighted
  [(RandomAppend Identity,   800),
   (RandomPrepend Identity,  200),

   (RandomAppend Shrinking,  c1),
   (RandomAppend Mutation,   c2),
   (RandomAppend Expansion,  c3),
   (RandomAppend Swapping,   c3),
   (RandomAppend Deletion,   c3),

   (RandomPrepend Shrinking, c1),
   (RandomPrepend Mutation,  c2),
   (RandomPrepend Expansion, c3),
   (RandomPrepend Swapping,  c3),
   (RandomPrepend Deletion,  c3),

   (RandomSplice,            c4),
   (RandomInterleave,        c4)
 ]

-- | At seqLen 1 there is nothing to extend or prepend to, so each sequence is
-- either a fresh transaction or a tweaked copy of a stored one.
seqMutatorsStateless
  :: MonadRandom m
  => MutationConsts Integer
  -> m CorpusMutation
seqMutatorsStateless (c1, c2, _, _) = weighted
  [(RandomAppend Identity,  500),
   (RandomAppend Mutation,  500 + c2),
   (RandomAppend Shrinking, c1)]
