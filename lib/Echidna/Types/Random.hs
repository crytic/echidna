module Echidna.Types.Random where

import Prelude hiding ((!!))
import Control.Monad.Random.Strict (MonadRandom, getRandomR, weighted)
import Data.List.NonEmpty ((!!), NonEmpty(..))

type Seed = Int

-- | Get a random element of a non-empty list.
rElem :: MonadRandom m => NonEmpty a -> m a
rElem l  = (l !!) <$> getRandomR (0, length l - 1)

oftenUsually :: MonadRandom m => a -> a -> m a
oftenUsually u r = weighted [(u, 10), (r, 1)]

usuallyRarely :: MonadRandom m => a -> a -> m a
usuallyRarely u r = weighted [(u, 100), (r, 1)]

usuallyVeryRarely :: MonadRandom m => a -> a -> m a
usuallyVeryRarely u r = weighted [(u, 1000), (r, 1)]
