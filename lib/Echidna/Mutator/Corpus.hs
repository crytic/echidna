module Echidna.Mutator.Corpus where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, weighted)
import Control.Monad.Reader (MonadReader, MonadIO)
import Control.Monad.ST (RealWorld)
import Data.Set (Set)
import Data.Set qualified as Set
import EVM.Types (VM, VMResult(..), VMType(..))

import Echidna.Mutator.Array
import Echidna.Transaction (mutateTx, shrinkTx)
import Echidna.Types (MutationConsts)
import Echidna.Types.Config (Env)
import Echidna.Types.Corpus
import Echidna.Exec (execTx)
import Echidna.Types.Tx (Tx)

defaultMutationConsts :: Num a => MutationConsts a
defaultMutationConsts = (1, 1, 1, 1, 1)

fromConsts :: Num a => MutationConsts Integer -> MutationConsts a
fromConsts (a, b, c, d, e) = let fi = fromInteger in (fi a, fi b, fi c, fi d, fi e)

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
                    | RemoveReverting
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
  :: (MonadRandom m, MonadIO m, MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> CorpusMutation
  -> (Int -> Corpus -> [Tx] -> m [Tx])
getCorpusMutation _ (RandomAppend m) = mut (mutator m)
  where
    mut f ql ctxs gtxs = do
      rtxs' <- selectAndMutate f ctxs
      pure . take ql $ rtxs' ++ gtxs
getCorpusMutation _ (RandomPrepend m) = mut (mutator m)
  where
    mut f ql ctxs gtxs = do
      rtxs' <- selectAndMutate f ctxs
      k <- getRandomR (0, ql - 1)
      pure . take ql $ take k gtxs ++ rtxs'
getCorpusMutation _ RandomSplice = selectAndCombine spliceAtRandom
getCorpusMutation _ RandomInterleave = selectAndCombine interleaveAtRandom
getCorpusMutation vmInitial RemoveReverting = const . const $ filterOutTxs vmInitial where
  filterOutTxs _ [] = pure []
  filterOutTxs vm (tx:rest) = do
    ((result, _), vm') <- execTx vm tx
    let append = case result of
                   VMSuccess _ -> [tx]
                   _ -> []
    (append <>) <$> filterOutTxs vm' rest

seqMutatorsStateful
  :: MonadRandom m
  => MutationConsts Rational
  -> m CorpusMutation
seqMutatorsStateful (c1, c2, c3, c4, c5) = weighted
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
   (RandomInterleave,        c4),

   (RemoveReverting,         c5)
 ]

seqMutatorsStateless
  :: MonadRandom m
  => MutationConsts Rational
  -> m CorpusMutation
seqMutatorsStateless (c1, c2, _, _, _) = weighted
  [(RandomAppend Identity,   800),
   (RandomPrepend Identity,  200),

   (RandomAppend Shrinking,  c1),
   (RandomAppend Mutation,   c2),

   (RandomPrepend Shrinking, c1),
   (RandomPrepend Mutation,  c2)
  ]
