module Echidna.Mutator.Array where

import Control.Monad.Random.Strict (fromList, MonadRandom, getRandomR)
import Data.ListLike qualified as LL

-- | A list of mutators to randomly select to perform a mutation of list-like values
listMutators :: (LL.ListLike f i, MonadRandom m) => m (f -> m f)
listMutators = fromList
  [ (pure, 1) -- no-op
  , (expandRandList, 10)
  , (deleteRandList, 10)
  , (swapRandList, 10)
  ]

-- | Mutate a list-like data structure using a list of mutators
mutateLL
  :: (LL.ListLike f i, MonadRandom m)
  -- | Required size for the mutated list-like value (or Nothing if there are no constraints)
  => Maybe Int
  -- | Randomly generated list-like value to complement the mutated list, if it is
  -- shorter than the requested size
  -> f
  -- | List-like value to mutate
  -> f
  -> m f
mutateLL mn fs vs = do
  f <- listMutators
  xs <- f vs
  return $ maybe xs (`LL.take` (xs <> fs)) mn

replaceAt :: LL.ListLike f i => i -> f -> Int -> f
replaceAt i f n = LL.take n f <> LL.cons i (LL.drop (n + 1) f)

expandAt :: LL.ListLike f i => f -> Int -> Int -> f
expandAt xs k t =
  case LL.uncons xs of
    Nothing -> xs
    Just (y,ys) ->
      if k == 0
         then LL.replicate t y <> ys
         else LL.cons y (expandAt ys (k - 1) t)

expandRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
expandRandList xs
  | l == 0    = return xs
  | l >= 32   = return xs
  | otherwise = do
    k <- getRandomR (0, l - 1)
    t <- getRandomR (1, min 32 l)
    return $ expandAt xs k t
  where l = LL.length xs

deleteAt :: LL.ListLike f i => Int -> f -> f
deleteAt n f = LL.take n f <> LL.drop (n+1) f

deleteRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
deleteRandList xs =
  if LL.null xs
  then return xs
  else do
    k <- getRandomR (0, LL.length xs - 1)
    return $ deleteAt k xs

-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: LL.ListLike f i => f -> Int -> Int -> f
swapAt xs i j = left <> LL.cons elemJ middle <> LL.cons elemI right
  where elemI = xs `LL.index` i
        elemJ = xs `LL.index` j
        left = LL.take i xs
        middle = LL.take (j - i - 1) (LL.drop (i + 1) xs)
        right = LL.drop (j + 1) xs

swapRandList :: (LL.ListLike f i, MonadRandom m) => f -> m f
swapRandList xs =
  if LL.null xs
  then return xs
  else do
    i <- getRandomR (0, LL.length xs - 1)
    j <- getRandomR (0, LL.length xs - 1)
    return $ if i == j then xs else swapAt xs (min i j) (max i j)

spliceAtRandom :: (LL.ListLike f i, MonadRandom m) => f -> f -> m f
spliceAtRandom xs1 xs2 = do
  idx1 <- getRandomR (0, LL.length xs1 - 1)
  idx2 <- getRandomR (0, LL.length xs2 - 1)
  return $ LL.take idx1 xs1 <> LL.drop idx2 xs2

interleaveAtRandom :: (LL.ListLike f i, MonadRandom m) => f -> f -> m f
interleaveAtRandom xs1 xs2 = do
  idx1 <- getRandomR (0, LL.length xs1 - 1)
  idx2 <- getRandomR (0, LL.length xs2 - 1)
  return $ LL.take idx1 xs1 `interleaveLL` LL.take idx2 xs2

{- | Takes two lists and combines them interleaving its elements -}
interleaveLL :: LL.ListLike f i => f -> f -> f
interleaveLL a b
  | LL.null a = b
  | LL.null b = a
  | otherwise = LL.cons (LL.head a) . LL.cons (LL.head b) $ interleaveLL (LL.tail a) (LL.tail b)
