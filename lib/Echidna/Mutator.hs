{-# LANGUAGE FlexibleContexts #-}

module Echidna.Mutator where

import Control.Monad.Random.Strict (fromList, MonadRandom, getRandomR)
import Data.Maybe (maybe)

import qualified Data.ListLike as LL

-- | A list of mutators to randomly select to perform a mutation of list-like values
listMutators :: (LL.ListLike f i, MonadRandom m) => m (f -> m f)
listMutators = fromList [(return, 1), (expandRandList, 10), (deleteRandList, 10), (swapRandList, 10)] 

-- | Mutate a list-like data structure using a list of mutators 
mutateLL :: (LL.ListLike f i, MonadRandom m)
         => Maybe Int -- ^ Required size for the mutated list-like value (or Nothing if there are no constrains)
         -> f         -- ^ Randomly generated list-like value to complement the mutated list, if it is shorter than the requested size
         -> f         -- ^ List-like value to mutate
         -> m f
mutateLL mn fs vs = do
  f <- listMutators
  xs <- f vs
  return $ maybe xs (`LL.take` (xs `LL.append` fs)) mn

replaceAt :: LL.ListLike f i => i -> f -> Int -> f
replaceAt i f n = LL.take n f `LL.append` LL.cons i (LL.drop (n+1) f)

expandAt :: LL.ListLike f i => f -> Int -> Int -> f
expandAt xs i n = case LL.uncons xs of 
                       Nothing     -> xs
                       Just (y,ys) -> if i == 0
                                      then LL.replicate n y `LL.append` ys
                                      else LL.cons y (expandAt ys (i-1) n)


expandRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
expandRandList xs = if LL.null xs 
                    then return xs
                    else do k <- getRandomR (0, LL.length xs - 1)
                            t <- getRandomR (1, max 32 (LL.length xs))
                            return $ expandAt xs k t


deleteAt :: LL.ListLike f i => Int -> f -> f
deleteAt n f = LL.take n f `LL.append` LL.drop (n+1) f

deleteRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
deleteRandList xs = if LL.null xs
                    then return xs 
                    else do k <- getRandomR (0, LL.length xs - 1)
                            return $ deleteAt k xs


-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: LL.ListLike f i => f -> Int -> Int -> f
swapAt xs i j =  let elemI = xs `LL.index` i
                     elemJ = xs `LL.index` j
                     left = LL.take i xs
                     middle = LL.take (j - i - 1) (LL.drop (i + 1) xs)
                     right = LL.drop (j + 1) xs
                 in  left `LL.append` LL.cons elemJ middle `LL.append` LL.cons elemI right

swapRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
swapRandList xs = if LL.null xs
                  then return xs
                  else do k1 <- getRandomR (0, LL.length xs - 1 )
                          k2 <- getRandomR (0, LL.length xs - 1 )
                          return $ swapAt xs k1 k2

spliceAtRandom :: (LL.ListLike f i, MonadRandom m) => f -> f -> m f
spliceAtRandom xs1 xs2 = do idx1 <- getRandomR (0, LL.length xs1 - 1) 
                            idx2 <- getRandomR (0, LL.length xs2 - 1)
                            return $ LL.take idx1 xs1 `LL.append` LL.drop idx2 xs2
