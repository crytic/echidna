module Echidna.Types.Coverage where

import Control.Monad ((>=>))
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), withText)
import Data.Bits (testBit)
import Data.Foldable (foldl', length, sum)
import Data.IORef (IORef, readIORef)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimVar (PrimVar, atomicReadInt)
import Data.Primitive.PrimArray
  (MutablePrimArray, PrimArray, freezePrimArray, indexPrimArray, newPrimArray, readPrimArray, setPrimArray)
import Data.Set qualified as Set
import Data.Text (toLower)
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as V
import Data.Word (Word64)
import GHC.Exts (RealWorld)
import Prelude hiding (Foldable(..))

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SolcContract(..))
import EVM.Types (W256)

import Echidna.Types.Tx (TxResult)

-- | Coverage of one code unit: the runtime code or the creation code of a
-- contract, identified by its compile-time codehash (see `CodehashMap`).
data CovEntry = CovEntry
  { bits :: !(MutablePrimArray RealWorld Int)
    -- ^ Two words per pc, shared by every worker: word @2*pc@ is the bitset of
    -- call depths the pc executed at, word @2*pc+1@ the bitset of 'TxResult's
    -- observed with the pc as the last executed instruction. A pc is covered
    -- iff its depth word is non-zero. Written only through the atomic
    -- primitives in "Echidna.Types.Coverage.Atomic".
  , opIxMap :: !(VS.Vector Int)
    -- ^ Byte index to op index of the code unit, borrowed from hevm's 'Contract'
  , len :: !Int
    -- ^ Number of pcs (bytes of code)
  }

-- | Map with the coverage information needed for fuzzing and source code printing.
-- Indexed by contracts' compile-time codehash; see `CodehashMap`.
type CoverageMap = Map W256 CovEntry

-- | Allocate a zeroed entry for a code unit of the given length.
newCovEntry :: VS.Vector Int -> Int -> IO CovEntry
newCovEntry opIxMap len = do
  bits <- newPrimArray (2 * len)
  setPrimArray bits 0 (2 * len) 0
  pure CovEntry { bits, opIxMap, len }

-- | Snapshot an entry into the per-pc tuples the report and JSON writers
-- consume. A pc that was never covered gets op index @-1@, as before.
freezeCovEntry :: CovEntry -> IO (V.Vector CoverageInfo)
freezeCovEntry entry = do
  frozen <- freezePrimArray entry.bits 0 (2 * entry.len) :: IO (PrimArray Int)
  pure $ V.generate entry.len $ \pc ->
    let depths = indexPrimArray frozen (2 * pc)
        results = indexPrimArray frozen (2 * pc + 1)
        opIx = if depths == 0 then -1 else fromMaybe 0 (entry.opIxMap VS.!? pc)
    in (opIx, fromIntegral depths, fromIntegral results)

-- | Number of covered pcs in an entry.
coveredPoints :: CovEntry -> IO Int
coveredPoints entry = go 0 0
  where
    go !acc !pc
      | pc >= entry.len = pure acc
      | otherwise = do
          depths <- readPrimArray entry.bits (2 * pc)
          go (if depths == 0 then acc else acc + 1) (pc + 1)

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
  where freeze = readIORef >=> mapM freezeCovEntry

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

-- | Point coverage (from the running counter) and the number of unique
-- contracts hit. Cheap enough for status lines and per-event use; the counter
-- may trail concurrent writers by a few increments.
coverageStats :: PrimVar RealWorld Int -> IORef CoverageMap -> IORef CoverageMap -> IO (Int, Int)
coverageStats pointsVar initRef runtimeRef = do
  points <- atomicReadInt pointsVar
  codehashes <- uniqueCodehashes initRef runtimeRef
  pure (points, codehashes)

-- | Like 'coverageStats' but recounts the covered pcs from the arrays; exact,
-- for the final report once the workers have stopped.
coverageStatsExact :: IORef CoverageMap -> IORef CoverageMap -> IO (Int, Int)
coverageStatsExact initRef runtimeRef = do
  pointsInit <- scoveragePoints =<< readIORef initRef
  pointsRuntime <- scoveragePoints =<< readIORef runtimeRef
  codehashes <- uniqueCodehashes initRef runtimeRef
  pure (pointsInit + pointsRuntime, codehashes)

uniqueCodehashes :: IORef CoverageMap -> IORef CoverageMap -> IO Int
uniqueCodehashes initRef runtimeRef = do
  initMap <- readIORef initRef
  runtimeMap <- readIORef runtimeRef
  pure $ length $ Set.fromList $ Map.keys initMap ++ Map.keys runtimeMap

-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful for reporting a coverage measure to the user
scoveragePoints :: CoverageMap -> IO Int
scoveragePoints cm = sum <$> mapM coveredPoints (Map.elems cm)

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

-- | Per-agent private coverage state. There is one slot per fuzz or symbolic
-- agent plus one for deployment-time coverage, allocated up front in
-- 'Echidna.mkEnv' and addressed by position, never by 'workerId': the symbolic
-- worker also runs as worker 0, so two agents can share a 'workerId'.
data CovSlot = CovSlot
  { slotIx :: !Int
    -- ^ Position in 'Env.coverageSlots'; the last position is the deployment slot
  }

-- | Allocate the slot at the given position.
newCovSlot :: Int -> IO CovSlot
newCovSlot ix = pure CovSlot { slotIx = ix }
