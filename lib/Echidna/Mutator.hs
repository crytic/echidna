{-# LANGUAGE FlexibleContexts #-}

module Echidna.Mutator where

import Control.Monad.Random.Strict (fromList, MonadRandom, getRandomR)
import Data.Maybe (maybe)

import qualified Data.ListLike as LL

listMutators :: (LL.ListLike f i, MonadRandom m) => m (f -> m f)
listMutators = fromList [(return, 1), (expandRandList, 10), (deleteRandList, 10), (swapRandList, 10)] 

mutateLL :: (LL.ListLike f i, MonadRandom m) => Maybe Int -> f -> f -> m f
mutateLL mn fs vs = do
  f <- listMutators
  xs <- f vs
  return $ maybe xs (`LL.take` (xs `LL.append` fs)) mn

--replaceAt _ []     _ = []
--replaceAt a (_:xs) 0 = a:xs
--replaceAt a (x:xs) n =
--  if n < 0
--  then x : xs
--  else x : replaceAt a xs (n-1)

replaceAt :: LL.ListLike f i => i -> f -> Int -> f
replaceAt i f n = LL.take n f `LL.append` LL.cons i (LL.drop (n+1) f)

--expandAt :: [a] -> Int -> Int -> [a]
--expandAt []     _ _ = []
--expandAt (x:xs) i n = case i of 
--                       0   -> replicate n x ++ xs 
--                       _   -> x: expandAt xs (i-1) n

expandAt :: LL.ListLike f i => f -> Int -> Int -> f
expandAt xs i n = case LL.uncons xs of 
                       Nothing     -> xs
                       Just (y,ys) -> if (i == 0) 
                                      then LL.replicate n y `LL.append` ys
                                      else LL.cons y (expandAt ys (i-1) n)


expandRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
expandRandList xs = if (LL.null xs) 
                    then return xs
                    else do k <- getRandomR (0, LL.length xs - 1)
                            t <- getRandomR (1, max 32 (LL.length xs))
                            return $ expandAt xs k t


--deleteAt :: Int -> [a] -> [a]
--deleteAt _ [] = []
--deleteAt i (a:as)
--   | i == 0    = as
--   | otherwise = a : deleteAt (i-1) as

deleteAt :: LL.ListLike f i => Int -> f -> f
deleteAt n f = LL.take n f `LL.append` LL.drop (n+1) f

deleteRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
deleteRandList xs = if (LL.null xs)
                    then return xs 
                    else do k <- getRandomR (0, LL.length xs - 1)
                            return $ deleteAt k xs


-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
--swapAt :: [a] -> Int -> Int -> [a]
--swapAt xs i j =  let elemI = xs !! i
--                     elemJ = xs !! j
--                     left = take i xs
--                     middle = take (j - i - 1) (drop (i + 1) xs)
--                     right = drop (j + 1) xs
--                 in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 

-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: LL.ListLike f i => f -> Int -> Int -> f
swapAt xs i j =  let elemI = xs `LL.index` i
                     elemJ = xs `LL.index` j
                     left = LL.take i xs
                     middle = LL.take (j - i - 1) (LL.drop (i + 1) xs)
                     right = LL.drop (j + 1) xs
                 in  left `LL.append` (LL.cons elemJ middle) `LL.append` (LL.cons elemI right)


swapRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
swapRandList xs = if (LL.null xs)
                  then return xs
                  else do k1 <- getRandomR (0, LL.length xs - 1 )
                          k2 <- getRandomR (0, LL.length xs - 1 )
                          return $ swapAt xs k1 k2
 
{-
listMutators :: MonadRandom m => m ([a] -> m [a])
listMutators = fromList [(return, 1), (expandRandList, 10), (deleteRandList, 10), (swapRandList, 10)] 
 
mutateV :: MonadRandom m => Maybe Int -> [AbiValue] -> [AbiValue] -> m [AbiValue]
mutateV mn fs vs = do
  f <- listMutators
  xs <- f vs
  return $ maybe xs (`take` (xs ++ fs)) mn

mutateBS :: MonadRandom m => Maybe Int -> [Word8] -> ByteString -> m ByteString
mutateBS mn fs bs = do
  f <- listMutators
  xs <- f $ unpack bs
  return $ pack $ maybe xs (`take` (xs ++ fs)) mn


replaceAt :: a -> [a] -> Int -> [a]
replaceAt _ []     _ = []
replaceAt a (_:xs) 0 = a:xs
replaceAt a (x:xs) n =
  if n < 0
  then x : xs
  else x : replaceAt a xs (n-1)


expandRandList :: MonadRandom m => [a] -> m [a]
expandRandList []   = return []
expandRandList xs = do k <- getRandomR (0, length xs - 1)
                       t <- getRandomR (1, max 32 (length xs))
                       return $ expandAt xs k t

expandAt :: [a] -> Int -> Int -> [a]
expandAt []     _ _ = []
expandAt (x:xs) i n = case i of 
                       0   -> replicate n x ++ xs 
                       _   -> x: expandAt xs (i-1) n

deleteRandList :: MonadRandom m => [a] -> m [a]
deleteRandList []   = return []
deleteRandList xs = do k <- getRandomR (0, length xs - 1)
                       return $ deleteAt k xs

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt i (a:as)
   | i == 0    = as
   | otherwise = a : deleteAt (i-1) as

insertAt :: a -> [a] -> Int -> [a]
insertAt v [] _ = [v]
insertAt v arr 1 = v : arr
insertAt v (x:xs) n = x : insertAt v xs (n - 1)

insertRandList :: MonadRandom m => [a] -> [a] -> m [a]
insertRandList xs [] = return xs
insertRandList xs (y:ys) = do idx <- getRandomR (0, length xs - 1) 
                              insertRandList (insertAt y xs idx) ys


swapRandList :: MonadRandom m => [a] -> m [a]
swapRandList []   = return []
swapRandList xs = do k1 <- getRandomR (0, length xs - 1 )
                     k2 <- getRandomR (0, length xs - 1 )
                     return $ swapAt xs k1 k2

-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: [a] -> Int -> Int -> [a]
swapAt xs i j =  let elemI = xs !! i
                     elemJ = xs !! j
                     left = take i xs
                     middle = take (j - i - 1) (drop (i + 1) xs)
                     right = drop (j + 1) xs
                 in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 

spliceAtRandom :: MonadRandom m => [a] -> [a] -> m [a]
spliceAtRandom xs1 xs2 = do idx1 <- getRandomR (0, length xs1 - 1) 
                            idx2 <- getRandomR (0, length xs2 - 1)
                            return $ take idx1 xs1 ++ drop idx2 xs2

-}
