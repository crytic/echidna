module Echidna.Types.Constraint where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)

import EVM.Types (Addr, W256)

import Echidna.Types.Tx (Tx)

-- | An inclusive interval discovered for a numeric function argument.
-- Missing endpoints mean that symbolic execution did not discover a bound.
data NumericBounds = NumericBounds
  { lowerBound :: Maybe Integer
  , upperBound :: Maybe Integer
  }
  deriving (Eq, Show)

-- | Bounds keyed by a zero-based argument index.
type ArgumentBounds = Map Int NumericBounds

-- | The concrete call context a bound was inferred for. The transaction
-- prefix is compared structurally, never hashed. Sender, destination, and
-- value belong to the target call because guards can depend on them too.
data ConstraintKey = ConstraintKey
  { prefix :: [Tx]
  , signature :: Text
  , sender :: Addr
  , destination :: Addr
  , callValue :: W256
  , callGas :: Word64
  , callGasPrice :: W256
  , callDelay :: (W256, W256)
  }
  deriving (Eq, Ord, Show)

-- | Bound the useful state contexts cached by each fuzzer for a signature.
-- Eviction lets later campaign states replace early discoveries.
maxContextsPerSignature :: Int
maxContextsPerSignature = 4

-- | Bound the number of queued, requested, and cached misses per signature.
-- Evicting from these small samples lets the discovery worker keep observing
-- states reached later in the campaign.
maxRecentContextsPerSignature :: Int
maxRecentContextsPerSignature = 4

-- | Clamp discovered bounds to an unsigned ABI type's legal range.
unsignedRange :: Int -> NumericBounds -> Maybe (Integer, Integer)
unsignedRange bits NumericBounds{lowerBound, upperBound}
  | lower <= upper = Just (lower, upper)
  | otherwise = Nothing
  where
    lower = max 0 $ fromMaybe 0 lowerBound
    upper = min maximumValue $ fromMaybe maximumValue upperBound
    maximumValue = 2 ^ bits - 1

-- | Clamp discovered bounds to a signed ABI type's legal range.
signedRange :: Int -> NumericBounds -> Maybe (Integer, Integer)
signedRange bits NumericBounds{lowerBound, upperBound}
  | lower <= upper = Just (lower, upper)
  | otherwise = Nothing
  where
    minimumValue = negate $ 2 ^ (bits - 1)
    maximumValue = 2 ^ (bits - 1) - 1
    lower = max minimumValue $ fromMaybe minimumValue lowerBound
    upper = min maximumValue $ fromMaybe maximumValue upperBound
