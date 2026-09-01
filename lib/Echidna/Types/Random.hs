module Echidna.Types.Random where

import Control.Monad.Random.Strict (MonadRandom, RandT, StdGen, getRandom, getRandomR, evalRandT, getStdGen, forM_, liftIO)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray, getElems)
import Data.List.NonEmpty ((!!), NonEmpty(..))
import Data.Set (Set)
import Data.Set qualified as S
import Data.Word (Word64)
import Prelude hiding ((!!))

type Seed = Int

-- | Get a random element of a non-empty list.
rElem :: MonadRandom m => NonEmpty a -> m a
rElem l  = (l !!) <$> getRandomR (0, length l - 1)

-- | Get a random element of a Set
rElem' :: MonadRandom m => Set a -> m a
rElem' v = (`S.elemAt` v) <$> getRandomR (0, length v - 1)

-- | Pick an element with probability proportional to its weight.
--
-- Makes the very same choice 'Control.Monad.Random.weighted' makes from the
-- same generator state -- the first element whose cumulative weight @c@
-- satisfies @c >= total * (1 - w / maxBound)@ for one uniformly drawn 'Word64'
-- @w@ -- so a seed still yields the same campaign. MonadRandom decides that in
-- 'Rational', and the gcd normalisation behind each step of it took about 3%
-- of a campaign's CPU time; rearranged to @total * w >= maxBound * (total - c)@
-- it is one 'Integer' comparison per element.
weighted :: MonadRandom m => [(a, Integer)] -> m a
weighted xs
  | total <= 0 = error "Echidna.Types.Random.weighted: empty list, or total weight <= 0"
  | otherwise = do
      w <- getRandom
      pure $ pick (total * toInteger (w :: Word64)) 0 xs
  where
  total = sum (map snd xs)
  maxW = toInteger (maxBound :: Word64)
  -- total > 0 rules out the empty list, and the last element always passes
  -- the test (its cumulative weight is the total), so this is exhaustive
  pick _ _ [] = error "Echidna.Types.Random.weighted: empty list"
  pick lhs c ((x, q) : rest)
    | lhs >= maxW * (total - c') = x
    | otherwise = pick lhs c' rest
    where c' = c + q

-- | Pick an element uniformly, making the same choice as
-- 'Control.Monad.Random.uniform' from the same generator state.
uniform :: MonadRandom m => [a] -> m a
uniform = weighted . map (, 1)

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