module Echidna.Types.Constraint where

import Data.Map (Map)
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

-- | Keep discovery bounded even when fuzzing produces many unique states.
maxContextsPerSignature :: Int
maxContextsPerSignature = 4

-- | Clamp discovered bounds to an unsigned ABI type's legal range.
unsignedRange :: Int -> NumericBounds -> Maybe (Integer, Integer)
unsignedRange bits NumericBounds{lowerBound, upperBound}
  | lower <= upper = Just (lower, upper)
  | otherwise = Nothing
  where
    lower = max 0 $ maybe 0 id lowerBound
    upper = min maximumValue $ maybe maximumValue id upperBound
    maximumValue = 2 ^ bits - 1
