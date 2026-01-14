module Echidna.Types.Coverage where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), withText)
import Data.Bits (testBit)
import Data.Foldable (foldl', length, sum)
import Data.IORef (IORef, readIORef)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Data.Text (toLower)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (IOVector)
import Data.Vector.Unboxed.Mutable qualified as VM
import Data.Word (Word64)
import Prelude hiding (Foldable(..))

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SolcContract(..))
import EVM.Types (W256)

import Echidna.Types.Tx (TxResult)

-- | Map with the coverage information needed for fuzzing and source code printing.
-- Indexed by contracts' compile-time codehash; see `CodehashMap`.
type CoverageMap = Map W256 (IOVector CoverageInfo)

-- | CoverageMap, but using Vectors instead of IOVectors.
-- IO is not required to access this map's members.
type FrozenCoverageMap = Map W256 (V.Vector CoverageInfo)

-- | Basic coverage information
type CoverageInfo = (OpIx, StackDepths, TxResults)

-- | Index per operation in the source code, obtained from the source mapping
type OpIx = Int

-- | Packed call stack depths from the EVM, corresponding bits are set
type StackDepths = Word64

-- | Packed TxResults used for coverage, corresponding bits are set
type TxResults = Word64

-- | Given the CoverageMaps used for contract init and runtime, produce a single combined coverage map
-- with op indices from init correctly shifted over (see srcMapForOpLocation in Echidna.Output.Source).
-- Takes IORef CoverageMap because this is how they are stored in the Env.
mergeCoverageMaps :: DappInfo -> IORef CoverageMap -> IORef CoverageMap -> IO FrozenCoverageMap
mergeCoverageMaps dapp initMap runtimeMap = mergeFrozenCoverageMaps dapp <$> freeze initMap <*> freeze runtimeMap
  where freeze = readIORef >=> mapM V.freeze

-- | Given the FrozenCoverageMaps used for contract init and runtime, produce a single combined coverage map
-- with op indices from init correctly shifted over (see srcMapForOpLocation in Echidna.Output.Source).
-- Helper function for mergeCoverageMaps.
mergeFrozenCoverageMaps :: DappInfo -> FrozenCoverageMap -> FrozenCoverageMap -> FrozenCoverageMap
mergeFrozenCoverageMaps dapp initMap runtimeMap = Map.unionWith (<>) runtimeMap initMap'
  where
    initMap' = Map.mapWithKey modifyInitMapEntry initMap
    -- eta reduced, second argument is a vec
    modifyInitMapEntry hash = V.map $ modifyCoverageInfo $ getOpOffset hash
    modifyCoverageInfo toAdd (op, x, y) = (op + toAdd, x, y)
    getOpOffset hash = maybe 0 (length . (.runtimeSrcmap) . snd) $ Map.lookup hash dapp.solcByHash

-- | Given the CoverageMaps used for contract init and runtime,
-- return the point coverage and the number of unique contracts hit.
-- Takes IORef CoverageMap because this is how they are stored in the Env.
coverageStats :: IORef CoverageMap -> IORef CoverageMap -> IO (Int, Int)
coverageStats initRef runtimeRef = do
  initMap <- readIORef initRef
  runtimeMap <- readIORef runtimeRef
  pointsInit <- scoveragePoints initMap
  pointsRuntime <- scoveragePoints runtimeMap
  pure (pointsInit + pointsRuntime, length $ Set.fromList $ Map.keys initMap ++ Map.keys runtimeMap)

-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful for reporting a coverage measure to the user
scoveragePoints :: CoverageMap -> IO Int
scoveragePoints cm = do
  sum <$> mapM (VM.foldl' countCovered 0) (Map.elems cm)

countCovered :: Int -> CoverageInfo -> Int
countCovered acc (opIx,_,_) = if opIx == -1 then acc else acc + 1

unpackTxResults :: TxResults -> [TxResult]
unpackTxResults txResults =
  foldl' (\results bit ->
    if txResults `testBit` bit
      then toEnum bit : results
      else results
  ) [] [0..63]

data CoverageFileType = Lcov | Html | Txt deriving (Eq, Show)

instance ToJSON CoverageFileType where
  toJSON = toJSON . show

instance FromJSON CoverageFileType where
  parseJSON = withText "CoverageFileType" $ readFn . toLower where
    readFn "lcov" = pure Lcov
    readFn "html" = pure Html
    readFn "text" = pure Txt
    readFn "txt"  = pure Txt
    readFn _ = fail "could not parse CoverageFileType"
