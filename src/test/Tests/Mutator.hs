module Tests.Mutator (mutatorTests) where

import Control.Monad.Random.Strict (evalRand, mkStdGen)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
  (Gen, Positive(..), arbitrary, choose, elements, forAll, listOf1, testProperty, vectorOf, (===))

import Common (overrideQuiet, runContract)
import Echidna.Config (defaultConfig)
import Echidna.Mutator.Corpus (CorpusMutation(..), TxsMutation(..), getCorpusMutation)
import Echidna.Types.Campaign
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Corpus (Corpus)
import Echidna.Types.Worker (WorkerType(..))
import Tests.Encoding () -- Arbitrary Tx

mutatorTests :: TestTree
mutatorTests = testGroup "Corpus mutation"
  [ testProperty "every mutation yields exactly seqLen transactions" $
      forAll genCorpus $ \corpus ->
      forAll (choose (1, 8)) $ \ql ->
      forAll (vectorOf ql arbitrary) $ \gtxs ->
      forAll (elements allMutations) $ \m ->
      forAll arbitrary $ \seed ->
        length (evalRand (getCorpusMutation m ql corpus gtxs) (mkStdGen seed)) === ql
  , testGroup "the test limit is spent on whole sequences"
      [ accountingTest 20 600
      , accountingTest 1 100
      ]
  ]

-- | Every corpus mutation. Neither enum derives 'Enum' or 'Bounded'.
allMutations :: [CorpusMutation]
allMutations =
  map RandomAppend txsMutations <> map RandomPrepend txsMutations
  <> [RandomSplice, RandomInterleave]
  where txsMutations = [Identity, Shrinking, Mutation, Expansion, Swapping, Deletion]

-- | A non-empty corpus of non-empty sequences. The keys are the selection
-- weights, so they are positive as in a real corpus.
genCorpus :: Gen Corpus
genCorpus = Set.fromList <$> listOf1 entry
  where entry = (,) . getPositive <$> arbitrary <*> listOf1 arbitrary

-- | A single fuzz worker with coverage on, no shrinking and a property that
-- never fails, so it runs until the limit. Every sequence it runs must then
-- have exactly seqLen transactions and be charged exactly that many calls.
accountingTest :: Int -> Int -> TestTree
accountingTest n limit =
  testCase ("seqLen " <> show n) $ do
    (env, final) <- runContract "basic/flags.sol" Nothing cfg FuzzWorker
    corpus <- readIORef env.corpusRef
    assertBool "corpus stayed empty, so no mutation was exercised" (not (Set.null corpus))
    final.ncalls @?= limit
    final.ncalls @?= final.ncallseqs * n
  where
    cfg = defaultConfig
      { campaignConf = defaultConfig.campaignConf
        { testLimit = limit
        , seqLen = n
        , stopOnFail = False
        , shrinkLimit = 0
        , seed = Just 0
        , corpusDir = Nothing
        }
      } & overrideQuiet
