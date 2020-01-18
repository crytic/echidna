module Echidna.Mutators where

import Control.Monad.Random.Strict (MonadRandom, getRandomR)

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt i (a:as)
   | i == 0    = as
   | otherwise = a : deleteAt (i-1) as

insertAt :: a -> [a] -> Int -> [a]
insertAt v [] _ = [v]
insertAt v arr 1 = (v:arr)
insertAt v (x:xs) n = (x:(insertAt v xs $ n - 1))

insertAtRandom :: MonadRandom m => [a] -> [a] -> m [a]
insertAtRandom xs [] = return xs
insertAtRandom xs (y:ys) = do idx <- getRandomR (0, (length xs) - 1) 
                              insertAtRandom (insertAt y xs idx) ys


-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: [a] -> Int -> Int -> [a]
swapAt xs i j =  let elemI = xs !! i
                     elemJ = xs !! j
                     left = take i xs
                     middle = take (j - i - 1) (drop (i + 1) xs)
                     right = drop (j + 1) xs
                 in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 


