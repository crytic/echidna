module Echidna.Types.Coverage where

import Data.Bits (testBit)
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Vector.Unboxed.Mutable qualified as V
import Data.Word (Word64)
import EVM.Types (W256)

import Echidna.Types.Tx (TxResult)

-- | Map with the coverage information needed for fuzzing and source code printing.
-- Indexed by contracts' compile-time codehash; see `CodehashMap`.
type CoverageMap = Map W256 (IOVector CoverageInfo)

-- | Basic coverage information
type CoverageInfo = (OpIx, StackDepths, TxResults)

-- | Index per operation in the source code, obtained from the source mapping
type OpIx = Int

-- | Packed call stack depths from the EVM, corresponding bits are set
type StackDepths = Word64

-- | Packed TxResults used for coverage, corresponding bits are set
type TxResults = Word64

-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful to report a coverage measure to the user
scoveragePoints :: CoverageMap -> IO Int
scoveragePoints cm = do
  sum <$> mapM (V.foldl' countCovered 0) (Map.elems cm)

countCovered :: Int -> CoverageInfo -> Int
countCovered acc (opIx,_,_) = if opIx == -1 then acc else acc + 1

unpackTxResults :: TxResults -> [TxResult]
unpackTxResults txResults =
  foldl' (\results bit ->
    if txResults `testBit` bit
      then toEnum bit : results
      else results
  ) [] [0..63]
