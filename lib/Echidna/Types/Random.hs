module Echidna.Types.Random where

import Prelude hiding ((!!))
import Control.Monad.Random.Strict (MonadRandom, RandT, StdGen, getRandomR, weighted, evalRandT, getStdGen, forM_, liftIO)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray, getElems)
import Data.List.NonEmpty ((!!), NonEmpty(..))
import Data.Set (Set)
import Data.Set qualified as S

type Seed = Int

-- | Get a random element of a non-empty list.
rElem :: MonadRandom m => NonEmpty a -> m a
rElem l  = (l !!) <$> getRandomR (0, length l - 1)

-- | Get a random element of a Set
rElem' :: MonadRandom m => Set a -> m a
rElem' v = (`S.elemAt` v) <$> getRandomR (0, length v - 1)

oftenUsually :: MonadRandom m => a -> a -> m a
oftenUsually u r = weighted [(u, 10), (r, 1)]

usuallyRarely :: MonadRandom m => a -> a -> m a
usuallyRarely u r = weighted [(u, 100), (r, 1)]

usuallyVeryRarely :: MonadRandom m => a -> a -> m a
usuallyVeryRarely u r = weighted [(u, 1000), (r, 1)]

-- Helper to explicitly type the array creation
createIOArray :: [a] -> IO (IOArray Int a)
createIOArray xs = newListArray (1, length xs) xs

-- Fisher-Yates shuffle using RandT + IOArray
shuffle :: [a] -> RandT StdGen IO [a]
shuffle xs = do
    arr <- liftIO $ createIOArray xs
    let n = length xs
    forM_ [n, n - 1 .. 2] $ \i -> do
        j <- getRandomR (1, i)
        liftIO $ swap arr i j
    liftIO $ getElems arr
  where
    swap arr i j = do
        vi <- readArray arr i
        vj <- readArray arr j
        writeArray arr i vj
        writeArray arr j vi

shuffleIO :: [a] -> IO [a]
shuffleIO xs = evalRandT (shuffle xs) =<< getStdGen