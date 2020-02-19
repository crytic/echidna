module Echidna.Mutator where

import Control.Monad.Random.Strict (fromList, MonadRandom, getRandomR)
--import Data.Monoid (Endo(..), appEndo)
import Data.Maybe (maybe)
import Data.ByteString (ByteString, pack, unpack)
import EVM.ABI (AbiValue(..))

mutateV :: MonadRandom m => Maybe Int -> [AbiValue] -> [AbiValue] -> m [AbiValue]
mutateV mn fs vs = do
  f <- fromList [(return, 1), (expandRandList, 3), (deleteRandList, 3), (swapRandList, 3)] 
  xs <- f vs
  return $ maybe xs (`take` (xs ++ fs)) mn

mutateBS :: MonadRandom m => Maybe Int -> ByteString -> m ByteString
mutateBS mn bs = do
  f <- fromList [(return, 1), (expandRandList, 3), (deleteRandList, 3), (swapRandList, 3)] 
  xs <- f $ unpack bs
  return $ pack $ maybe xs (`take` (xs ++ repeat 0)) mn


replaceAt :: a -> [a] -> Int -> [a]
replaceAt _ []     _ = []
replaceAt a (_:xs) 0 = a:xs
replaceAt a (x:xs) n =
  if n < 0
    then (x:xs)
    else x: replaceAt a xs (n-1)


expandRandList :: MonadRandom m => [a] -> m [a]
expandRandList []   = return []
expandRandList rtxs = do k <- getRandomR (0, length rtxs - 1)
                         return $ insertAt (rtxs !! k) rtxs k

expandAt :: [a] -> Int -> Int -> [a]
expandAt []     _ _ = []
expandAt (x:xs) i n = case i of 
                       0   -> replicate n x ++ xs 
                       _   -> x: expandAt xs (i-1) n

deleteRandList :: MonadRandom m => [a] -> m [a]
deleteRandList []   = return []
deleteRandList rtxs = do k <- getRandomR (0, length rtxs - 1)
                         return $ deleteAt k rtxs

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
insertRandList xs (y:ys) = do idx <- getRandomR (0, (length xs) - 1) 
                              insertRandList (insertAt y xs idx) ys


swapRandList :: MonadRandom m => [a] -> m [a]
swapRandList []   = return []
swapRandList rtxs = do k1 <- getRandomR (0, (length rtxs) - 1 )
                       k2 <- getRandomR (0, (length rtxs) - 1 )
                       return $ swapAt rtxs k1 k2

-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: [a] -> Int -> Int -> [a]
swapAt xs i j =  let elemI = xs !! i
                     elemJ = xs !! j
                     left = take i xs
                     middle = take (j - i - 1) (drop (i + 1) xs)
                     right = drop (j + 1) xs
                 in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 

spliceAtRandom :: MonadRandom m => [a] -> [a] -> m [a]
spliceAtRandom xs1 xs2 = do idx1 <- getRandomR (0, (length xs1) - 1) 
                            idx2 <- getRandomR (0, (length xs2) - 1)
                            return $ take idx1 xs1 ++ drop idx2 xs2

--applyAll :: [a -> a] -> a -> a
--applyAll = appEndo . mconcat . map Endo
