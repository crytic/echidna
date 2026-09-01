module Echidna.Mutator.Corpus where

import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Data.Maybe (maybeToList)
import Data.Set qualified as Set

import Echidna.Mutator.Array
import Echidna.Transaction (forceMutateTx, mutateTx, shrinkTx)
import Echidna.Types (MutationConsts)
import Echidna.Types.Corpus
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
  -> Corpus
  -> m [Tx]
selectAndMutate m corpus = do
  rtxs <- selectFromCorpus corpus
  k <- getRandomR (cutRange m (length rtxs))
  mutator m $ take k rtxs

selectAndCombine
  :: MonadRandom m
  => ([Tx] -> [Tx] -> m [Tx])
  -> Int
  -> Corpus
  -> [Tx]
  -> m [Tx]
selectAndCombine f ql corpus gtxs = do
  rtxs1 <- selectFromCorpus corpus
  rtxs2 <- selectFromCorpus corpus
  txs <- f rtxs1 rtxs2
  pure . take ql $ txs <> gtxs

selectFromCorpus
  :: MonadRandom m
  => Corpus
  -> m [Tx]
selectFromCorpus =
  weighted . map (\(i, txs) -> (txs, fromIntegral i)) . Set.toDescList

getCorpusMutation
  :: MonadRandom m
  => CorpusMutation
  -> (Int -> Corpus -> [Tx] -> m [Tx])
getCorpusMutation (RandomAppend m) = \ql ctxs gtxs -> do
  rtxs' <- selectAndMutate m ctxs
  pure . take ql $ rtxs' ++ gtxs
getCorpusMutation (RandomPrepend m) = \ql ctxs gtxs -> do
  rtxs' <- selectAndMutate m ctxs
  k <- getRandomR (0, ql - 1)
  -- Pad with the remaining fresh transactions so the sequence has ql entries.
  pure . take ql $ take k gtxs ++ rtxs' ++ drop k gtxs
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
