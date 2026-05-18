module Tests.Sample (sampleTests) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import EVM.ABI (AbiValue(..))

import Echidna.Types.Campaign
  ( SampleStats(..)
  , abiCompare
  , applySampleEvent
  , emptySampleStats
  , maxRecentReverts
  , mergeSampleStats
  )
import Echidna.Types.Tx (TxResult(..))

uint :: Integer -> AbiValue
uint = AbiUInt 256 . fromInteger

int :: Integer -> AbiValue
int = AbiInt 256 . fromInteger

addr :: Integer -> AbiValue
addr = AbiAddress . fromInteger

sampleTests :: TestTree
sampleTests = testGroup "Sample stats"
  [ abiCompareTests
  , applySampleEventTests
  , mergeSampleStatsTests
  ]

-- ---------------------------------------------------------------------------
-- abiCompare
-- ---------------------------------------------------------------------------

abiCompareTests :: TestTree
abiCompareTests = testGroup "abiCompare"
  [ testCase "uint equal"   $ abiCompare (uint 5)    (uint 5)    @?= Just EQ
  , testCase "uint less"    $ abiCompare (uint 1)    (uint 5)    @?= Just LT
  , testCase "uint greater" $ abiCompare (uint 10)   (uint 5)    @?= Just GT
  , testCase "int negative compared" $
      abiCompare (int (-1))  (int 1)     @?= Just LT
  , testCase "address ordering" $
      abiCompare (addr 0x1)  (addr 0x10) @?= Just LT
  , testCase "bool False < True" $
      abiCompare (AbiBool False) (AbiBool True) @?= Just LT
  , testCase "mismatched kinds (uint vs bool) is incomparable" $
      abiCompare (uint 1) (AbiBool True) @?= Nothing
  , testCase "string is incomparable (unsupported kind)" $
      abiCompare (AbiString "a") (AbiString "b") @?= Nothing
  ]

-- ---------------------------------------------------------------------------
-- applySampleEvent — the per-event algebra
-- ---------------------------------------------------------------------------

applySampleEventTests :: TestTree
applySampleEventTests = testGroup "applySampleEvent"
  [ testCase "success bumps calls only" $ do
      let s = applySampleEvent ReturnTrue Nothing "foo" [] emptySampleStats
      s.sampleCalls          @?= 1
      s.sampleReverts        @?= 0
      s.sampleRecentReverts  @?= []
      s.sampleReturnRange    @?= Nothing

  , testCase "revert bumps reverts and pushes summary" $ do
      let s = applySampleEvent ErrorRevert Nothing "transfer" [uint 100] emptySampleStats
      s.sampleCalls         @?= 1
      s.sampleReverts       @?= 1
      case s.sampleRecentReverts of
        [summary] -> do
          assertBool "summary mentions function" (T.isPrefixOf "transfer(" summary)
          assertBool "summary mentions result kind" (T.isSuffixOf "ErrorRevert" summary)
        _ -> assertBool "expected exactly one revert summary" False

  , testCase "ReturnFalse counts as success (not a revert)" $ do
      let s = applySampleEvent ReturnFalse Nothing "ok" [] emptySampleStats
      s.sampleReverts @?= 0
      s.sampleCalls   @?= 1

  , testCase "Stop counts as success" $ do
      let s = applySampleEvent Stop Nothing "ok" [] emptySampleStats
      s.sampleReverts @?= 0
      s.sampleCalls   @?= 1

  , testCase "return value seeds the range on first success" $ do
      let s = applySampleEvent ReturnTrue (Just (uint 42)) "totalSupply" [] emptySampleStats
      s.sampleReturnRange @?= Just (uint 42, uint 42)

  , testCase "return value widens range (smaller value)" $ do
      let s0 = emptySampleStats { sampleReturnRange = Just (uint 10, uint 20) }
          s1 = applySampleEvent ReturnTrue (Just (uint 5)) "x" [] s0
      s1.sampleReturnRange @?= Just (uint 5, uint 20)

  , testCase "return value widens range (larger value)" $ do
      let s0 = emptySampleStats { sampleReturnRange = Just (uint 10, uint 20) }
          s1 = applySampleEvent ReturnTrue (Just (uint 100)) "x" [] s0
      s1.sampleReturnRange @?= Just (uint 10, uint 100)

  , testCase "return value inside range leaves range unchanged" $ do
      let s0 = emptySampleStats { sampleReturnRange = Just (uint 10, uint 20) }
          s1 = applySampleEvent ReturnTrue (Just (uint 15)) "x" [] s0
      s1.sampleReturnRange @?= Just (uint 10, uint 20)

  , testCase "return value not provided keeps existing range" $ do
      let s0 = emptySampleStats { sampleReturnRange = Just (uint 1, uint 2) }
          s1 = applySampleEvent ReturnTrue Nothing "x" [] s0
      s1.sampleReturnRange @?= Just (uint 1, uint 2)

  , testCase "recent reverts capped at maxRecentReverts (newest first)" $ do
      let go i = applySampleEvent ErrorRevert Nothing ("fn" <> T.pack (show i)) []
          final = foldr go emptySampleStats [1 .. maxRecentReverts + 5 :: Int]
      length final.sampleRecentReverts @?= maxRecentReverts
      case final.sampleRecentReverts of
        (top:_) -> assertBool "newest revert summary is at head" (T.isPrefixOf "fn1(" top)
        []      -> assertBool "expected non-empty" False
  ]

-- ---------------------------------------------------------------------------
-- mergeSampleStats — combining across workers
-- ---------------------------------------------------------------------------

mergeSampleStatsTests :: TestTree
mergeSampleStatsTests = testGroup "mergeSampleStats"
  [ testCase "counts sum" $ do
      let a = emptySampleStats { sampleCalls = 3, sampleReverts = 1 }
          b = emptySampleStats { sampleCalls = 5, sampleReverts = 2 }
          m = mergeSampleStats a b
      m.sampleCalls   @?= 8
      m.sampleReverts @?= 3

  , testCase "range widens to envelope" $ do
      let a = emptySampleStats { sampleReturnRange = Just (uint 10, uint 50) }
          b = emptySampleStats { sampleReturnRange = Just (uint 5,  uint 30) }
          m = mergeSampleStats a b
      m.sampleReturnRange @?= Just (uint 5, uint 50)

  , testCase "range preserved when one side has no range" $ do
      let a = emptySampleStats { sampleReturnRange = Just (uint 1, uint 2) }
          b = emptySampleStats
          m = mergeSampleStats a b
      m.sampleReturnRange @?= Just (uint 1, uint 2)

  , testCase "both sides without a range stay None" $ do
      let m = mergeSampleStats emptySampleStats emptySampleStats
      m.sampleReturnRange @?= Nothing

  , testCase "recent reverts concatenated and capped" $ do
      let aList = map (\i -> "a" <> T.pack (show i)) [1 .. maxRecentReverts :: Int]
          bList = map (\i -> "b" <> T.pack (show i)) [1 .. maxRecentReverts :: Int]
          a = emptySampleStats { sampleRecentReverts = aList }
          b = emptySampleStats { sampleRecentReverts = bList }
          m = mergeSampleStats a b
      length m.sampleRecentReverts @?= maxRecentReverts
      -- 'a' side comes first since merge concatenates a ++ b
      case m.sampleRecentReverts of
        (top:_) -> assertBool "first entry from 'a' side" (T.isPrefixOf "a" top)
        []      -> assertBool "expected non-empty" False
  ]
