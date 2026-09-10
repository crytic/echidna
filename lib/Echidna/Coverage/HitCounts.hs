-- | Per-line hit counts: how many times each pc executed, and how many of those
-- executions were inside transactions that failed.
--
-- Counts are private to a 'CovSlot' (one per agent). During a transaction each
-- executed pc bumps a transaction-local counter (a private, cache-hot load and
-- store); a pc is appended to the slot's touched list the first time its
-- counter leaves zero, so committing visits only what ran. On completion the
-- transaction's counts are folded into the permanent arrays under 'mask_' (and
-- into the failed-execution array too if the transaction failed), so the report
-- never sees a partially counted transaction.
module Echidna.Coverage.HitCounts
  ( slotEntryFor
  , recordPc
  , beginTx
  , commitTx
  , unitBytes
  , maxCommitWork
  , HitCountSnapshot(..)
  , snapshotHitCounts
  ) where

import Control.Exception (mask_)
import Control.Monad (forM, forM_, unless, when)
import Data.Foldable (toList)
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray
  (copyMutablePrimArray, freezePrimArray, indexPrimArray, newPrimArray, readPrimArray, setPrimArray, writePrimArray)
import Data.Primitive.PrimVar (newPrimVar, readPrimVar, writePrimVar)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray, smallArrayFromListN)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU

import EVM.Solidity (CodeType)
import EVM.Types (W256)

import Echidna.Types.Coverage (CovEntry(..), CovSlot(..), SlotEntry(..), SlotState(..))

-- | Payload bytes of the hit-count state one slot allocates for a code unit:
-- three 'Int' counters and one 'Word32' list slot per pc.
unitBytes :: Int -> Int
unitBytes len = 28 * len

-- | Most touched pcs one transaction may commit under 'mask_'; beyond it the
-- transaction's counts are dropped and its units marked incomplete. Touched pcs
-- are distinct per unit, so exceeding this needs more than ~40 maximum-size
-- (24 KiB) units in one transaction.
maxCommitWork :: Int
maxCommitWork = 2 ^ (20 :: Int)

-- | The slot's hit-count state for a code unit, created on first sight. The
-- unit's dense index is stable for the slot's lifetime.
slotEntryFor :: CovSlot -> CovEntry -> IO (SlotEntry, Int)
slotEntryFor slot entry = do
  st <- readIORef slot.slotState
  case Map.lookup (entry.kind, entry.key) st.entries of
    Just ix -> pure (indexSmallArray st.arrays ix, ix)
    Nothing -> do
      new <- newSlotEntry entry
      let ix = sizeofSmallArray st.arrays
          arrays' = smallArrayFromListN (ix + 1) (toList st.arrays ++ [new])
      -- The registered-entries buffer grows with the entry table and keeps the
      -- indices registered so far in this transaction.
      touched' <- newPrimArray (ix + 1)
      n <- readPrimVar slot.nTouchedEntries
      when (n > 0) $ copyMutablePrimArray touched' 0 st.touchedEntries 0 n
      -- One write publishes the table, the arrays and the buffer together.
      mask_ $ writeIORef slot.slotState SlotState
        { entries = Map.insert (entry.kind, entry.key) ix st.entries
        , arrays = arrays'
        , touchedEntries = touched'
        }
      pure (new, ix)

newSlotEntry :: CovEntry -> IO SlotEntry
newSlotEntry entry = do
  let n = entry.len
      zeroed k = do arr <- newPrimArray k; setPrimArray arr 0 k 0; pure arr
  execCount <- zeroed n
  failedCount <- zeroed n
  txCount <- zeroed n
  touched <- newPrimArray n
  nTouched <- newPrimVar 0
  regTx <- newPrimVar 0
  countsIncomplete <- newPrimVar 0
  pure SlotEntry { unit = (entry.kind, entry.key), len = n, execCount, failedCount, txCount
                 , touched, nTouched, regTx, countsIncomplete }

-- | Count one execution of a pc in the transaction-local state of the slot.
-- Register-then-mutate order throughout, so temporary state is always
-- reachable from the registered lists.
recordPc :: CovSlot -> SlotEntry -> Int -> Int -> IO ()
recordPc slot se ix pc = do
  reg <- readPrimVar se.regTx
  when (reg == 0) $ do
    st <- readIORef slot.slotState
    n <- readPrimVar slot.nTouchedEntries
    writePrimArray st.touchedEntries n ix
    writePrimVar slot.nTouchedEntries (n + 1)
    writePrimVar se.regTx 1
  c <- readPrimArray se.txCount pc
  when (c == 0) $ do
    n <- readPrimVar se.nTouched
    writePrimArray se.touched n (fromIntegral pc)
    writePrimVar se.nTouched (n + 1)
  writePrimArray se.txCount pc (c + 1)
{-# INLINE recordPc #-}

-- | Start a transaction on the slot. Temporary state left behind by a
-- transaction that never completed is discarded first; no current caller
-- reuses a slot after an interruption, this is insurance.
beginTx :: CovSlot -> IO ()
beginTx slot = do
  n <- readPrimVar slot.nTouchedEntries
  when (n > 0) $ walkRegistered slot False False

-- | Finish a transaction on the slot: fold its temporary counts into the
-- permanent ones (and the failed counts if it failed) under 'mask_', unless
-- the walk would exceed 'maxCommitWork', in which case the transaction's counts
-- are dropped and its units flagged incomplete.
commitTx :: CovSlot -> Bool -> IO ()
commitTx slot failed = do
  n <- readPrimVar slot.nTouchedEntries
  when (n > 0) $ do
    st <- readIORef slot.slotState
    work <- sum <$> forM [0 .. n - 1] (\k -> do
      ix <- readPrimArray st.touchedEntries k
      readPrimVar (indexSmallArray st.arrays ix).nTouched)
    if work <= maxCommitWork
      then mask_ (walkRegistered slot True failed)
      else walkRegistered slot False True

-- | Visit every registered entry's touched pcs, committing them if asked, then
-- clear the temporary state. With @commit = False@ and @flag = True@ the units
-- are marked incomplete instead.
walkRegistered :: CovSlot -> Bool -> Bool -> IO ()
walkRegistered slot commit flagOrFailed = do
  st <- readIORef slot.slotState
  n <- readPrimVar slot.nTouchedEntries
  forM_ [0 .. n - 1] $ \k -> do
    ix <- readPrimArray st.touchedEntries k
    let se = indexSmallArray st.arrays ix
    m <- readPrimVar se.nTouched
    forM_ [0 .. m - 1] $ \j -> do
      pc <- fromIntegral <$> readPrimArray se.touched j
      when commit $ do
        c <- readPrimArray se.txCount pc
        readPrimArray se.execCount pc >>= writePrimArray se.execCount pc . (+ c)
        when flagOrFailed $ readPrimArray se.failedCount pc >>= writePrimArray se.failedCount pc . (+ c)
      writePrimArray se.txCount pc 0
    unless commit $ when flagOrFailed $ writePrimVar se.countsIncomplete 1
    writePrimVar se.nTouched 0
    writePrimVar se.regTx 0
  writePrimVar slot.nTouchedEntries 0

-- | Committed hit counts of one code unit, summed over the slots.
data HitCountSnapshot = HitCountSnapshot
  { execs :: VU.Vector Int
    -- ^ executions of each pc inside completed transactions
  , failedExecs :: VU.Vector Int
    -- ^ those executions that were inside failed transactions
  , incomplete :: Bool
    -- ^ some slot dropped a transaction's counts for this unit
  }

-- | Snapshot the committed counts of every code unit any slot has seen.
snapshotHitCounts :: V.Vector CovSlot -> IO (Map (CodeType, W256) HitCountSnapshot)
snapshotHitCounts slots = do
  perSlot <- forM (V.toList slots) $ \slot -> do
    st <- readIORef slot.slotState
    forM (Map.toList st.entries) $ \(unit, ix) -> do
      let se = indexSmallArray st.arrays ix
          n = se.len
      e <- freezePrimArray se.execCount 0 n
      f <- freezePrimArray se.failedCount 0 n
      inc <- readPrimVar se.countsIncomplete
      pure (unit, HitCountSnapshot (VU.generate n (indexPrimArray e)) (VU.generate n (indexPrimArray f)) (inc /= 0))
  pure $ Map.fromListWith merge (concat perSlot)
  where
    merge a b = HitCountSnapshot (VU.zipWith (+) a.execs b.execs) (VU.zipWith (+) a.failedExecs b.failedExecs) (a.incomplete || b.incomplete)
