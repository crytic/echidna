module Echidna.Mutation where

import Control.Monad.Random.Strict
import Data.Maybe (maybe)
import Data.ByteString (ByteString, pack, unpack)

mutateBS :: MonadRandom m => Maybe Int -> ByteString -> m ByteString
mutateBS mn bs = do
                 f <- fromList [(identity, 1), (modify,3), (expand, 3), (delete, 3), (swap, 3)] 
                 xs <- f $ unpack bs
                 return $ pack $ maybe xs (`take` (xs ++ repeat 0)) mn

                where modify [] = return []
                      modify xs = do
                                   i <- getRandomR (0, length xs - 1)
                                   a <- getRandom
                                   return $ modify' xs i a
                      expand [] = return []
                      expand xs = do
                                   i <- getRandomR (0, length xs - 1)
                                   m <- getRandomR (0, length xs * 2) 
                                   return $ expand' xs i m
                      delete [] = return []
                      delete xs = getRandomR (0, length xs - 1) >>= \i -> return $ delete' xs i 
                      swap [] = return []
                      swap xs = do
                                   i <- getRandomR (0, length xs - 1)
                                   j <- getRandomR (0, length xs - 1) 
                                   return $ swap' xs i j

                      identity = return


modify' :: [a] -> Int -> a -> [a]
modify' []     _ _ = [] 
modify' (x:xs) i a = case i of 
                      0   -> a:xs 
                      _   -> x: modify' xs (i-1) a


expand' :: [a] -> Int -> Int -> [a]
expand' []     _ _ = []
expand' (x:xs) i n = case i of 
                       0   -> replicate n x ++ xs 
                       _   -> x: expand' xs (i-1) n

delete' :: [a] -> Int -> [a]
delete' []  _ = []
delete' xs  i = ys ++ tail zs
                  where (ys,zs) = splitAt i xs  

-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swap' :: [a] -> Int -> Int -> [a]
swap' xs i j =  let elemI = xs !! i
                    elemJ = xs !! j
                    left = take i xs
                    middle = take (j - i - 1) (drop (i + 1) xs)
                    right = drop (j + 1) xs
                in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 
