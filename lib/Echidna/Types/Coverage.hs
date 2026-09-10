module Echidna.Types.Coverage where

import Control.Monad (forM, void, when)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON), Value(..), withText)
import Data.Bits (bit, shiftL, shiftR, testBit, xor, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (foldl', length, sum)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimArray
  (MutablePrimArray, PrimArray, freezePrimArray, indexPrimArray, newPrimArray, primArrayFromListN, readPrimArray, setPrimArray)
import Data.Primitive.PrimVar (PrimVar, atomicReadInt, fetchAddInt, newPrimVar)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray)
import Data.Set qualified as Set
import Data.Text (toLower)
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as V
import Data.Word (Word32, Word64, Word8)
import GHC.Exts (RealWorld)
import Prelude hiding (Foldable(..))

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (CodeType, SolcContract(..))
import EVM.Types (W256)

import Echidna.Types.Coverage.Atomic (fetchOrPrimArray)
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
  , owner :: !W256
    -- ^ `runtimeCodehash` of the contract this unit belongs to: the key itself
    -- for runtime code, the owning contract's runtime hash for creation code
    -- (or the key when no compiled contract matched). Report and JSON output
    -- group units by owner; "unique codehashes" counts distinct owners.
  , kind :: !CodeType
    -- ^ Runtime or creation code
  , key :: !W256
    -- ^ The unit's compile-time codehash, its key in the coverage map
  , countsEligible :: !Bool
    -- ^ Whether slots keep hit counts for this unit: decided once, at creation,
    -- from the code size and the hit-count byte budget (see 'HitCountsMode')
  , code :: !(PrimArray Word8)
    -- ^ The unit's bytecode, exactly 'len' bytes, kept only when edge coverage
    -- is on so the loop can tell which pcs are jumps; empty otherwise
  , edges :: !(Maybe (MutablePrimArray RealWorld Int))
    -- ^ Edge coverage bitmap (see 'recordEdge'), when on and the code is concrete
  }

-- | Map with the coverage information needed for fuzzing and source code
-- printing. Indexed by the code unit's compile-time codehash (`runtimeCodehash`
-- in the runtime map, `creationCodehash` in the creation map); see `CodehashMap`.
type CoverageMap = Map W256 CovEntry

-- | Allocate a zeroed entry for a code unit. Edge coverage needs the concrete
-- bytecode; without it the unit records no edges.
newCovEntry :: CodeType -> W256 -> W256 -> Bool -> Bool -> Maybe ByteString -> VS.Vector Int -> Int -> IO CovEntry
newCovEntry kind key owner countsEligible edgesOn bytes opIxMap len = do
  bits <- newPrimArray (2 * len)
  setPrimArray bits 0 (2 * len) 0
  (code, edges) <- case bytes of
    Just bs | edgesOn -> do
      bm <- newPrimArray edgeWords
      setPrimArray bm 0 edgeWords 0
      -- pad a short buffer with INVALID, which is never a jump
      pure (primArrayFromListN len (take len (BS.unpack bs ++ repeat 0xfe)), Just bm)
    _ -> pure (primArrayFromListN 0 [], Nothing)
  pure CovEntry { bits, opIxMap, len, owner, kind, key, countsEligible, code, edges }

-- * Edge coverage

-- | Words in a unit's edge bitmap: 2^16 bits, AFL-style, collisions accepted.
edgeWords :: Int
edgeWords = 1024

-- | Whether the opcode byte is JUMP or JUMPI.
isJumpOp :: Word8 -> Bool
isJumpOp b = b == 0x56 || b == 0x57
{-# INLINE isJumpOp #-}

-- | Record the jump edge @(src, dst)@ in a unit's bitmap: a plain read, and an
-- atomic OR only when the bit is missing, whose old value says whether this
-- worker saw the edge first. Returns whether the edge was new.
recordEdge :: PrimVar RealWorld Int -> MutablePrimArray RealWorld Int -> Int -> Int -> IO Bool
recordEdge counter bm src dst = do
  let h = ((src `shiftL` 1) `xor` dst) .&. 0xffff
      w = h `shiftR` 6
      b = h .&. 63
  word <- readPrimArray bm w
  if word `testBit` b then pure False else do
    old <- fetchOrPrimArray bm w (bit b)
    let new = not (old `testBit` b)
    when new $ void $ fetchAddInt counter 1
    pure new

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

-- | Snapshot every code unit's per-pc tuples, keyed by unit, with op indices
-- relative to the unit's own source map (unlike 'mergeCoverageMaps').
snapshotUnits :: IORef CoverageMap -> IORef CoverageMap -> IO (Map (CodeType, W256) (V.Vector CoverageInfo))
snapshotUnits initRef runtimeRef = do
  initMap <- readIORef initRef
  runtimeMap <- readIORef runtimeRef
  Map.fromList <$> forM (Map.elems initMap ++ Map.elems runtimeMap) (\e -> ((e.kind, e.key),) <$> freezeCovEntry e)

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

-- | Given the CoverageMaps used for contract init and runtime, produce a single
-- combined coverage map keyed by owning contract (its `runtimeCodehash`), with
-- the creation-code vectors appended after the runtime vector and their op
-- indices shifted past the runtime source map (see srcMapForOpLocation in
-- Echidna.Output.Source). Takes IORef CoverageMap because this is how they are
-- stored in the Env.
mergeCoverageMaps :: DappInfo -> IORef CoverageMap -> IORef CoverageMap -> IO FrozenCoverageMap
mergeCoverageMaps dapp initRef runtimeRef = do
  runtimeFrozen <- traverse freezeCovEntry =<< readIORef runtimeRef
  initEntries <- Map.elems <$> readIORef initRef
  initFrozen <- Map.fromListWith (flip (<>)) <$> forM initEntries (\entry -> do
    vec <- freezeCovEntry entry
    pure (entry.owner, V.map (shiftOpIx (getOpOffset entry.owner)) vec))
  pure $ Map.unionWith (<>) runtimeFrozen initFrozen
  where
    shiftOpIx toAdd (op, x, y) = (op + toAdd, x, y)
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

-- | Number of distinct contracts with coverage, counting a contract's runtime
-- and creation code once.
uniqueCodehashes :: IORef CoverageMap -> IORef CoverageMap -> IO Int
uniqueCodehashes initRef runtimeRef = do
  initMap <- readIORef initRef
  runtimeMap <- readIORef runtimeRef
  pure $ length $ Set.fromList $ map (.owner) $ Map.elems initMap ++ Map.elems runtimeMap

-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful for reporting a coverage measure to the user
scoveragePoints :: CoverageMap -> IO Int
scoveragePoints cm = sum <$> mapM coveredPoints (Map.elems cm)

unpackTxResults :: TxResults -> [TxResult]
unpackTxResults txResults =
  foldl' (\results b ->
    if txResults `testBit` b
      then toEnum b : results
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

-- * Hit counts

-- | Whether to keep per-line hit counts, config key @coverageHitCounts@.
-- Memory for hit counts grows with slots x eligible bytecode, so @auto@ keeps
-- counting under a fixed payload budget and larger units fall back to bit
-- coverage only.
data HitCountsMode = HitCountsAuto | HitCountsOn | HitCountsOff
  deriving (Eq, Show)

instance FromJSON HitCountsMode where
  parseJSON = \case
    Bool True -> pure HitCountsOn
    Bool False -> pure HitCountsOff
    String "auto" -> pure HitCountsAuto
    _ -> fail "coverageHitCounts must be true, false or \"auto\""

-- | Payload budget in bytes for hit-count state, all slots together.
hitCountBudgetFor :: HitCountsMode -> Int
hitCountBudgetFor = \case
  HitCountsAuto -> 1024 * 1024 * 1024
  HitCountsOn -> maxBound
  HitCountsOff -> 0

-- | Hit-count state of one slot for one code unit, one counter per pc (see
-- "Echidna.Coverage.HitCounts").
data SlotEntry = SlotEntry
  { unit :: !(CodeType, W256)
  , len :: !Int
  , execCount :: !(MutablePrimArray RealWorld Int)
    -- ^ committed: executions of each pc
  , failedCount :: !(MutablePrimArray RealWorld Int)
    -- ^ committed: executions inside failed transactions
  , txCount :: !(MutablePrimArray RealWorld Int)
    -- ^ the current transaction's executions of each pc; zero between transactions
  , touched :: !(MutablePrimArray RealWorld Word32)
    -- ^ pcs whose 'txCount' went from zero this transaction, each at most once
  , nTouched :: !(PrimVar RealWorld Int)
  , regTx :: !(PrimVar RealWorld Int)
    -- ^ 1 once the unit is registered in the slot's list for this transaction
  , countsIncomplete :: !(PrimVar RealWorld Int)
    -- ^ 1 once a transaction's counts had to be dropped; the unit reports
    -- unknown counts from then on
  }

-- | A slot's table of units, published as one value so its parts agree.
data SlotState = SlotState
  { entries :: !(Map (CodeType, W256) Int)
    -- ^ unit -> index into 'arrays'
  , arrays :: !(SmallArray SlotEntry)
  , touchedEntries :: !(MutablePrimArray RealWorld Int)
    -- ^ indices registered in the current transaction; capacity = size of 'arrays'
  }

-- | Per-agent private coverage state. There is one slot per fuzz or symbolic
-- agent plus one for deployment-time coverage, allocated up front in
-- 'Echidna.mkEnv' and addressed by position, never by 'workerId': the symbolic
-- worker also runs as worker 0, so two agents can share a 'workerId'.
data CovSlot = CovSlot
  { slotIx :: !Int
    -- ^ Position in 'Env.coverageSlots'; the last position is the deployment slot
  , slotState :: !(IORef SlotState)
  , nTouchedEntries :: !(PrimVar RealWorld Int)
    -- ^ > 0 while a transaction is in flight (or was abandoned)
  }

-- | Allocate the slot at the given position.
newCovSlot :: Int -> IO CovSlot
newCovSlot ix = do
  touchedEntries <- newPrimArray 0
  slotState <- newIORef SlotState { entries = Map.empty, arrays = emptySmallArray, touchedEntries }
  nTouchedEntries <- newPrimVar 0
  pure CovSlot { slotIx = ix, slotState, nTouchedEntries }
