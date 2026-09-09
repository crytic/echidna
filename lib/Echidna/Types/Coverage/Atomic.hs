{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Lock-free atomic operations on @MutablePrimArray RealWorld Int@: thin
-- wrappers over the GHC primops, which imply a full memory barrier and do not
-- allocate. The shared coverage bit arrays use plain reads on the common path
-- and these on the rare novelty path.
module Echidna.Types.Coverage.Atomic
  ( fetchOrPrimArray
  , fetchAddPrimArray
  , atomicReadPrimArray
  , assertWordSize
  ) where

import Control.Monad (unless)
import Data.Bits (finiteBitSize)
import Data.Primitive.PrimArray (MutablePrimArray(..))
import GHC.Exts (Int(I#), RealWorld, atomicReadIntArray#, fetchAddIntArray#, fetchOrIntArray#)
import GHC.IO (IO(IO))

-- | Atomically OR a value into the word at an index; returns the previous value.
fetchOrPrimArray :: MutablePrimArray RealWorld Int -> Int -> Int -> IO Int
fetchOrPrimArray (MutablePrimArray mba) (I# i) (I# v) = IO $ \s ->
  case fetchOrIntArray# mba i v s of (# s', old #) -> (# s', I# old #)
{-# INLINE fetchOrPrimArray #-}

-- | Atomically add a value to the word at an index; returns the previous value.
fetchAddPrimArray :: MutablePrimArray RealWorld Int -> Int -> Int -> IO Int
fetchAddPrimArray (MutablePrimArray mba) (I# i) (I# v) = IO $ \s ->
  case fetchAddIntArray# mba i v s of (# s', old #) -> (# s', I# old #)
{-# INLINE fetchAddPrimArray #-}

-- | Read the word at an index with a full barrier.
atomicReadPrimArray :: MutablePrimArray RealWorld Int -> Int -> IO Int
atomicReadPrimArray (MutablePrimArray mba) (I# i) = IO $ \s ->
  case atomicReadIntArray# mba i s of (# s', v #) -> (# s', I# v #)
{-# INLINE atomicReadPrimArray #-}

-- | The coverage bitsets pack 64 call depths, or the 'Echidna.Types.Tx.TxResult'
-- constructors, into one 'Int' word; refuse to start on anything narrower.
assertWordSize :: IO ()
assertWordSize =
  unless (finiteBitSize (0 :: Int) == 64) $
    error "Echidna requires a 64-bit Int for its coverage bitsets"
