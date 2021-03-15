module Echidna.Types.Coverage where

import Control.Lens
import Data.Set (Set, size, map)
import Data.ByteString (ByteString) 
import Data.Map.Strict (Map)

import Echidna.Types.Tx (TxResult)

-- Program Counter directly obtained from the EVM
type PC = Int
-- Index per operation in the source code, obtained from the source mapping 
type OpIx = Int
-- Basic coverage information
type CoverageInfo = (PC, OpIx, TxResult)
-- Map with the coverage information needed for fuzzing and source code printing 
type CoverageMap = Map ByteString (Set CoverageInfo)

-- | Given good point coverage, count unique points.
coveragePoints :: CoverageMap -> Int
coveragePoints = sum . fmap size
-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful to report a coverage measure to the user
scoveragePoints :: CoverageMap -> Int
scoveragePoints = sum . fmap (size . Data.Set.map (view _1))


