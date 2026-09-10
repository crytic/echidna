module Tests.HitCounts (hitCountTests) where

import Control.Monad (forM_, replicateM_)
import Data.IORef (readIORef)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimVar (readPrimVar)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as VU
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import EVM.Solidity (CodeType(..))

import Echidna.Coverage.HitCounts
  (HitCountSnapshot(..), beginTx, commitTx, maxCommitWork, recordPc, slotEntryFor, snapshotHitCounts)
import Echidna.Types.Coverage (CovEntry(..), CovSlot(..), SlotEntry(..), SlotState(..), newCovEntry, newCovSlot)

hitCountTests :: TestTree
hitCountTests = testGroup "Hit counts"
  [ testCase "executions become per-pc counts on commit" $ do
      (slot, a) <- unit 1 10
      forM_ [0, 1, 2, 3, 5, 6, 7] (rec slot a)
      commitTx slot False
      snap <- snapshotOf slot a
      VU.toList snap.execs @?= [1, 1, 1, 1, 0, 1, 1, 1, 0, 0]
      VU.toList snap.failedExecs @?= replicate 10 0
      snap.incomplete @?= False

  , testCase "a failed transaction counts in both arrays" $ do
      (slot, a) <- unit 1 6
      forM_ [1 .. 4] (rec slot a)
      commitTx slot True
      snap <- snapshotOf slot a
      VU.toList snap.execs @?= [0, 1, 1, 1, 1, 0]
      VU.toList snap.failedExecs @?= [0, 1, 1, 1, 1, 0]

  , testCase "loop iterations accumulate and touch each pc once" $ do
      (slot, a) <- unit 1 5
      replicateM_ 3 $ forM_ [2, 3, 4] (rec slot a)
      (se, _) <- slotEntryFor slot a
      readPrimVar se.nTouched >>= (@?= 3)
      commitTx slot False
      snap <- snapshotOf slot a
      VU.toList snap.execs @?= [0, 0, 3, 3, 3]

  , testCase "an abandoned transaction contributes nothing to the next" $ do
      (slot, a) <- unit 1 6
      forM_ [0 .. 5] (rec slot a)   -- never committed
      beginTx slot                 -- next transaction on the same slot
      readPrimVar slot.nTouchedEntries >>= (@?= 0)
      rec slot a 2
      commitTx slot False
      snap <- snapshotOf slot a
      VU.toList snap.execs @?= [0, 0, 1, 0, 0, 0]

  , testCase "discovering a unit mid-transaction keeps earlier registrations" $ do
      slot <- newCovSlot 0
      a <- entry Runtime 1 4
      b <- entry Runtime 2 4
      c <- entry Creation 3 4
      forM_ [0, 1] (rec slot a)
      forM_ [0 .. 3] (rec slot b)
      forM_ [1, 2] (rec slot c)   -- grows the slot's table while a and b are registered
      forM_ [0, 1] (rec slot a)
      st <- readIORef slot.slotState
      Map.size st.entries @?= 3
      commitTx slot False
      snaps <- snapshotHitCounts (V.singleton slot)
      VU.toList (snaps Map.! (Runtime, 1)).execs @?= [2, 2, 0, 0]
      VU.toList (snaps Map.! (Runtime, 2)).execs @?= [1, 1, 1, 1]
      VU.toList (snaps Map.! (Creation, 3)).execs @?= [0, 1, 1, 0]

  , testCase "counts are summed across slots" $ do
      s1 <- newCovSlot 0
      s2 <- newCovSlot 1
      a <- entry Runtime 1 3
      forM_ [0 .. 2] (rec s1 a) >> commitTx s1 False
      forM_ [1 .. 2] (rec s2 a) >> commitTx s2 True
      snaps <- snapshotHitCounts (V.fromList [s1, s2])
      VU.toList (snaps Map.! (Runtime, 1)).execs @?= [1, 2, 2]
      VU.toList (snaps Map.! (Runtime, 1)).failedExecs @?= [0, 1, 1]

  , testCase "a transaction over the commit work bound is dropped and flagged" $ do
      let len = maxCommitWork + 8
      (slot, a) <- unit 1 len
      forM_ [0 .. maxCommitWork] (rec slot a)
      commitTx slot False
      snap <- snapshotOf slot a
      snap.incomplete @?= True
      VU.sum snap.execs @?= 0
      readPrimVar slot.nTouchedEntries >>= (@?= 0)
      -- the slot is clean afterwards
      rec slot a 0
      commitTx slot False
      snap' <- snapshotOf slot a
      VU.head snap'.execs @?= 1
      assertBool "still flagged" snap'.incomplete
  ]
  where
    entry kind key len = newCovEntry kind key key True False Nothing (VS.replicate len 0) len
    unit key len = do
      slot <- newCovSlot 0
      a <- entry Runtime key len
      pure (slot, a)
    rec slot e pc = do
      (se, ix) <- slotEntryFor slot e
      recordPc slot se ix pc
    snapshotOf slot e = do
      snaps <- snapshotHitCounts (V.singleton slot)
      pure (snaps Map.! (e.kind, e.key))
