module Echidna.Mutator.Corpus where

import Control.Monad.Random.Strict (MonadRandom, getRandomR, weighted)
import Data.Set (Set)
import Data.Set qualified as Set

import Echidna.Mutator.Array
import Echidna.Transaction (mutateTx, shrinkTx)
import Echidna.Types (MutationConsts)
import Echidna.Types.Tx (Tx)
import Echidna.Types.Corpus

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
mutator Mutation = mapM mutateTx
mutator Expansion = expandRandList
mutator Swapping = swapRandList
mutator Deletion = deleteRandList

selectAndMutate
  :: MonadRandom m
  => ([Tx] -> m [Tx])
  -> Corpus
  -> m [Tx]
selectAndMutate f corpus = do
  rtxs <- selectFromCorpus corpus
  k <- getRandomR (0, length rtxs - 1)
  f $ take k rtxs

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
  => Set (Int, [Tx])
  -> m [Tx]
selectFromCorpus =
  weighted . map (\(i, txs) -> (txs, fromIntegral i)) . Set.toDescList

getCorpusMutation
  :: MonadRandom m
  => CorpusMutation
  -> (Int -> Corpus -> [Tx] -> m [Tx])
getCorpusMutation (RandomAppend m) = mut (mutator m)
  where
    mut f ql ctxs gtxs = do
      rtxs' <- selectAndMutate f ctxs
      pure . take ql $ rtxs' ++ gtxs
getCorpusMutation (RandomPrepend m) = mut (mutator m)
  where
    mut f ql ctxs gtxs = do
      rtxs' <- selectAndMutate f ctxs
      k <- getRandomR (0, ql - 1)
      pure . take ql $ take k gtxs ++ rtxs'
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

seqMutatorsStateless
  :: MonadRandom m
  => MutationConsts Rational
  -> m CorpusMutation
seqMutatorsStateless (c1, c2, _, _) = weighted
  [(RandomAppend Identity,   800),
   (RandomPrepend Identity,  200),

   (RandomAppend Shrinking,  c1),
   (RandomAppend Mutation,   c2),

   (RandomPrepend Shrinking, c1),
   (RandomPrepend Mutation,  c2)
  ]
