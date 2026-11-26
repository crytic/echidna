module Echidna.SymExec.Calldata
  ( symbolicBufSize
  , minimumCalldataSize
  , calldataSizeError
  ) where

import EVM.ABI (AbiKind(..), AbiType(..), abiKind)
import EVM.Solidity (Method(..))

import Echidna.Types (ExecException(..))

-- | hevm constrains symbolic buffer lengths to be strictly less than 2^n.
symbolicBufSize :: Int
symbolicBufSize = 12

-- | Minimum canonical ABI encoding, including the four-byte selector. Dynamic
-- values may be empty, but still need their offsets and length words. Integer
-- arithmetic avoids overflow without constructing potentially enormous arrays.
minimumCalldataSize :: [AbiType] -> Integer
minimumCalldataSize types = 4 + sum (map argumentSize types)
  where
    argumentSize = \case
      AbiBytesDynamicType -> 64
      AbiStringType -> 64
      AbiArrayDynamicType _ -> 64
      t@(AbiArrayType count element) -> offset t + toInteger count * argumentSize element
      t@(AbiTupleType elements) -> offset t + sum (argumentSize <$> elements)
      _ -> 32
    offset t = if abiKind t == Dynamic then 32 else 0

calldataSizeError :: Method -> Maybe ExecException
calldataSizeError method
  | required >= limit = Just $ SymbolicCalldataTooLarge method.methodSignature required limit
  | otherwise = Nothing
  where
    required = minimumCalldataSize (map snd method.inputs)
    limit = 2 ^ symbolicBufSize
