module Tests.Atomic (atomicTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, replicateM, replicateM_)
import Data.Bits (bit, complement, testBit, (.|.))
import Data.Foldable (foldl')
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, setPrimArray)
import GHC.Exts (RealWorld)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (choose, forAll, ioProperty, listOf, testProperty)

import Echidna.Types.Coverage.Atomic (atomicReadPrimArray, fetchAddPrimArray, fetchOrPrimArray)

atomicTests :: TestTree
atomicTests = testGroup "Coverage atomics"
  [ testCase "fetchOr returns the previous value and accumulates" $ do
      arr <- zeroed 1
      fetchOrPrimArray arr 0 5 >>= (@?= 0)
      fetchOrPrimArray arr 0 3 >>= (@?= 5)
      readPrimArray arr 0 >>= (@?= 7)
      atomicReadPrimArray arr 0 >>= (@?= 7)

  , testCase "fetchAdd returns the previous value and accumulates" $ do
      arr <- zeroed 2
      fetchAddPrimArray arr 1 4 >>= (@?= 0)
      fetchAddPrimArray arr 1 (-1) >>= (@?= 4)
      readPrimArray arr 1 >>= (@?= 3)
      readPrimArray arr 0 >>= (@?= 0)

  , testCase "bit 63 (the sign bit) round-trips" $ do
      arr <- zeroed 1
      fetchOrPrimArray arr 0 (bit 63) >>= (@?= 0)
      v <- readPrimArray arr 0
      v @?= minBound
      testBit v 63 @?= True
      fetchOrPrimArray arr 0 (bit 63) >>= (@?= minBound)

  , testCase "exactly one of N racing threads observes the bit absent" $
      replicateM_ 200 $ do
        arr <- zeroed 1
        olds <- inParallel 8 $ fetchOrPrimArray arr 0 (bit 3)
        length (filter (not . (`testBit` 3)) olds) @?= 1
        readPrimArray arr 0 >>= (@?= bit 3)

  , testCase "64 threads setting disjoint bits converge to all ones" $
      replicateM_ 20 $ do
        arr <- zeroed 1
        _ <- inParallelIx 64 $ \i -> fetchOrPrimArray arr 0 (bit i)
        readPrimArray arr 0 >>= (@?= complement 0)

  , testProperty "fetchOr old values agree with a sequential model" $
      forAll (listOf ((,) <$> choose (0, 7) <*> choose (0, 1023))) $ \ops ->
        ioProperty $ do
          arr <- zeroed 8
          olds <- forM ops $ \(i, v) -> fetchOrPrimArray arr i v
          let model = scanl step (replicate 8 0) ops
              step ws (i, v) = [if j == i then w .|. v else w | (j, w) <- zip [0 ..] ws]
              expected = zipWith (\ws (i, _) -> ws !! i) model ops
          final <- forM [0 .. 7] (readPrimArray arr)
          pure (olds == expected && final == foldl' step (replicate 8 0) ops)
  ]
  where
    zeroed :: Int -> IO (MutablePrimArray RealWorld Int)
    zeroed n = do
      arr <- newPrimArray n
      setPrimArray arr 0 n 0
      pure arr

    -- Run the same action on n threads, collecting the results.
    inParallel :: Int -> IO a -> IO [a]
    inParallel n act = inParallelIx n (const act)

    inParallelIx :: Int -> (Int -> IO a) -> IO [a]
    inParallelIx n act = do
      vars <- replicateM n newEmptyMVar
      forM_ (zip [0 ..] vars) $ \(i, var) -> forkIO (act i >>= putMVar var)
      mapM takeMVar vars
