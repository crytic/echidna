module Echidna.Mutator.Corpus where

import Control.Monad.Random.Strict (MonadRandom, getRandomR, weighted)
import Data.Maybe (maybeToList)
import Data.Set qualified as Set

import Echidna.Mutator.Array
import Echidna.Transaction (forceMutateTx, mutateTx, shrinkTx)
import Echidna.Types (MutationConsts)
import Echidna.Types.Corpus
import Echidna.Types.Tx (Tx)

defaultMutationConsts :: Num a => MutationConsts a
defaultMutationConsts = (1, 1, 1, 1)

fromConsts :: Num a => MutationConsts Integer -> MutationConsts a
fromConsts (a, b, c, d) = let fi = fromInteger in (fi a, fi b, fi c, fi d)

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
  -- 'mutateTx' leaves most transactions alone so that a long prefix keeps most
  -- of its shape, and 'mutateAbiValue' changes an integer only one time in ten.
  -- On a single transaction that is almost always a verbatim replay, which at
  -- seqLen 1 (every sequence starts from the same VM) cannot find anything.
  -- Force a change instead, and drop the transaction when nothing in it can
  -- change: the caller pads the sequence back to length with fresh
  -- transactions, so the iteration still runs a useful transaction.
  [tx] -> maybeToList <$> forceMutateTx tx
  txs -> mapM mutateTx txs
mutator Expansion = expandRandList
mutator Swapping = swapRandList
mutator Deletion = deleteRandList

-- | The range the cut point is drawn from when a stored sequence of the given
-- length is handed to a mutation. Identity gets a strict prefix: replaying a
-- sequence unchanged finds nothing new, and the empty prefix is the "fresh
-- sequence" case. Every other mutation needs something to work on, so its
-- prefix is non-empty and may be the whole sequence.
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
  -- The fresh transactions not placed in front pad the sequence back to full
  -- length, as in RandomAppend. Without them a short stored prefix made the
  -- whole sequence shorter than seqLen, and at seqLen 1 (where k is always 0)
  -- an empty prefix came out as an empty sequence.
  pure . take ql $ take k gtxs ++ rtxs' ++ drop k gtxs
getCorpusMutation RandomSplice = selectAndCombine spliceAtRandom
getCorpusMutation RandomInterleave = selectAndCombine interleaveAtRandom

seqMutatorsStateful
  :: MonadRandom m
  => MutationConsts Rational
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

-- | At seqLen 1 a stored sequence is a single transaction, so the stateful
-- vocabulary collapses: there is no prefix to extend and nothing to prepend to,
-- and Identity can only mean a fresh transaction, since replaying the stored
-- one unchanged from the same initial VM finds nothing. Each iteration is
-- therefore either a fresh transaction or a tweaked copy of a stored one, with
-- the mutConsts knobs adding to the tweak share as in the stateful table.
seqMutatorsStateless
  :: MonadRandom m
  => MutationConsts Rational
  -> m CorpusMutation
seqMutatorsStateless (c1, c2, _, _) = weighted
  [(RandomAppend Identity,  500),
   (RandomAppend Mutation,  500 + c2),
   (RandomAppend Shrinking, c1)]
