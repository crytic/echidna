{-# LANGUAGE FlexibleContexts #-}

module Echidna.Mutator.Corpus where

import Control.Monad.Random.Strict (MonadRandom, getRandomR, weighted)
import Control.Monad.State.Strict (MonadState(..))
import Data.Has (Has(..))

import qualified Data.Set as DS

import Echidna.Types.Tx (Tx)
import Echidna.Types.Corpus
import Echidna.Transaction (mutateTx, shrinkTx)
import Echidna.ABI (GenDict)
import Echidna.Mutator.Array

type MutationConsts = (Integer, Integer, Integer, Integer, Integer)
defaultMutationConsts :: MutationConsts 
defaultMutationConsts = (10,1,1,1,1)

data TxsMutation = TxIdentity
                 | Shrinking
                 | Mutation
                 | Expansion
                 | Swapping
                 | Deletion

data CorpusMutation = Skip
                    | RandomAppend TxsMutation 
                    | RandomPrepend TxsMutation
                    | RandomSplice
                    | RandomInterleave

selectAndMutate :: MonadRandom m 
                => ([Tx] -> m [Tx]) -> Corpus -> m [Tx]
selectAndMutate f ctxs = do
  rtxs <- weighted $ map (\(i, txs) -> (txs, fromInteger i)) $ DS.toDescList ctxs
  k <- getRandomR (0, length rtxs - 1)
  f $ take k rtxs

selectAndCombine ::  MonadRandom m
                 => ([Tx] -> [Tx] -> m [Tx]) -> Int -> Corpus -> [Tx] -> m [Tx]
selectAndCombine f ql ctxs gtxs = do
  rtxs1 <- selectFromCorpus
  rtxs2 <- selectFromCorpus
  txs <- f rtxs1 rtxs2
  return . take ql $ txs ++ gtxs
    where selectFromCorpus = weighted $ map (\(i, txs) -> (txs, fromInteger i)) $ DS.toDescList ctxs

getCorpusMutation :: (MonadRandom m, Has GenDict x, MonadState x m) 
                  => CorpusMutation -> (Int -> Corpus -> [Tx] -> m [Tx])
getCorpusMutation Skip = \_ _ -> return
getCorpusMutation (RandomAppend m) = 
  case m of
    TxIdentity -> mut return
    Shrinking  -> mut (mapM shrinkTx)
    Mutation   -> mut (mapM mutateTx)
    Expansion  -> mut expandRandList
    Swapping   -> mut swapRandList
    Deletion   -> mut deleteRandList
 where mut f ql ctxs gtxs = do
          rtxs' <- selectAndMutate f ctxs
          return . take ql $ rtxs' ++ gtxs  

getCorpusMutation (RandomPrepend m) = 
  case m of
    TxIdentity -> mut return
    Shrinking  -> mut $ mapM shrinkTx
    Mutation   -> mut $ mapM mutateTx
    Expansion  -> mut expandRandList
    Swapping   -> mut swapRandList
    Deletion   -> mut deleteRandList
 where mut f ql ctxs gtxs = do
          rtxs' <- selectAndMutate f ctxs
          k <- getRandomR (0, ql - 1)
          return . take ql $ take k gtxs ++ rtxs'

getCorpusMutation RandomSplice = selectAndCombine spliceAtRandom
getCorpusMutation RandomInterleave = selectAndCombine interleaveAtRandom

seqMutators :: MonadRandom m => MutationConsts -> m CorpusMutation
seqMutators (c1, c2, c3, c4, c5) = weighted 
  [(Skip,                     1000),

   (RandomAppend TxIdentity,  fromInteger c1),
   (RandomAppend Shrinking,   fromInteger c2),
   (RandomAppend Mutation,    fromInteger c3),
   (RandomAppend Expansion,   fromInteger c4),
   (RandomAppend Swapping,    fromInteger c4),
   (RandomAppend Deletion,    fromInteger c4),

   (RandomPrepend TxIdentity, fromInteger c1),
   (RandomPrepend Shrinking,  fromInteger c2),
   (RandomPrepend Mutation,   fromInteger c3),
   (RandomPrepend Expansion,  fromInteger c4),
   (RandomPrepend Swapping,   fromInteger c4),
   (RandomPrepend Deletion,   fromInteger c4),

   (RandomSplice,             fromInteger c5),
   (RandomInterleave,         fromInteger c5)
 ]
