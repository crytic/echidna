module Tests.Mutator (mutatorTests) where

import Control.Monad (forM_)
import Control.Monad.Random.Strict (evalRand, mkStdGen)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck
  (Gen, Positive(..), arbitrary, choose, counterexample, elements, forAll, listOf1, property,
   testProperty, vectorOf, (===))

import EVM.ABI (AbiValue(..))

import Common (overrideQuiet, runContract)
import Echidna.ABI (forceMutateAbiValue, isMutable)
import Echidna.Config (defaultConfig)
import Echidna.Mutator.Corpus (CorpusMutation(..), TxsMutation(..), cutRange, getCorpusMutation)
import Echidna.Types.Campaign
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Corpus (Corpus)
import Echidna.Types.Tx (Tx(..), TxCall(..))
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
  , testGroup "cut point"
      [ testCase "identity keeps a strict prefix" $ do
          cutRange Identity 5 @?= (0, 4)
          cutRange Identity 1 @?= (0, 0)
          cutRange Identity 0 @?= (0, 0)
      , testCase "other mutations get a non-empty prefix, up to the whole sequence" $ do
          cutRange Mutation 5 @?= (1, 5)
          cutRange Mutation 1 @?= (1, 1)
          cutRange Shrinking 0 @?= (0, 0)
      ]
  , testGroup "seqLen 1"
      [ testProperty "a stored transaction is tweaked or replaced, never replayed" $
          forAll (elements [RandomAppend Mutation, RandomPrepend Mutation]) $ \m ->
          forAll arbitrary $ \seed ->
            case evalRand (getCorpusMutation m 1 singleton [fresh]) (mkStdGen seed) of
              [tx] | tx == fresh -> property True
                   | otherwise ->
                       counterexample (show tx) $ tx /= stored && fnName tx == fnName stored
              txs -> counterexample (show txs) False
      , testProperty "a stored transaction with nothing to change is replaced" $
          forAll (elements [mkTx "stored" [], mkTx "stored" [AbiAddress 0]]) $ \s ->
          forAll arbitrary $ \seed ->
            evalRand (getCorpusMutation (RandomAppend Mutation) 1 (Set.singleton (1, [s])) [fresh])
                     (mkStdGen seed)
              === [fresh]
      , testProperty "identity yields the fresh transaction" $
          forAll arbitrary $ \seed ->
            evalRand (getCorpusMutation (RandomAppend Identity) 1 singleton [fresh]) (mkStdGen seed)
              === [fresh]
      ]
  , testGroup "forced mutation"
      [ testProperty "never returns the original value" $
          forAll arbitrary $ \v -> forAll arbitrary $ \seed ->
            let r = evalRand (forceMutateAbiValue v) (mkStdGen seed)
            in counterexample (show r) $ r /= Just v
      , testCase "flips booleans" $
          forM_ [0 .. 20] $ \seed ->
            evalRand (forceMutateAbiValue (AbiBool True)) (mkStdGen seed) @?= Just (AbiBool False)
      , testCase "leaves addresses alone" $ do
          evalRand (forceMutateAbiValue (AbiAddress 0)) (mkStdGen 0) @?= Nothing
          isMutable (AbiAddress 0) @?= False
          isMutable (AbiUInt 8 1) @?= True
      ]
  , testGroup "the test limit is spent on whole sequences"
      [ accountingTest 20 600
      , accountingTest 1 100
      ]
  ]

-- | A seqLen 1 corpus: one single-transaction sequence.
singleton :: Corpus
singleton = Set.singleton (1, [stored])

-- | The stored transaction takes an integer, the argument type the ABI mutators
-- change least often. The fresh one is distinguishable by name.
stored, fresh :: Tx
stored = mkTx "stored" [AbiInt 8 5]
fresh = mkTx "fresh" []

mkTx :: Text -> [AbiValue] -> Tx
mkTx name args =
  Tx { call = SolCall (name, args), src = 0, dst = 0, gas = 0, gasprice = 0, value = 0, delay = (0, 0) }

fnName :: Tx -> Maybe Text
fnName tx = case tx.call of
  SolCall (name, _) -> Just name
  _ -> Nothing

-- | Every corpus mutation. Neither enum derives 'Enum' or 'Bounded'.
allMutations :: [CorpusMutation]
allMutations =
  map RandomAppend txsMutations <> map RandomPrepend txsMutations
  <> [RandomSplice, RandomInterleave]
  where txsMutations = [Identity, Shrinking, Mutation, Expansion, Swapping, Deletion]

-- | A non-empty corpus of non-empty sequences with positive weights.
genCorpus :: Gen Corpus
genCorpus = Set.fromList <$> listOf1 entry
  where entry = (,) . getPositive <$> arbitrary <*> listOf1 arbitrary

-- | Run a single worker to the test limit and check that every sequence had
-- exactly seqLen transactions and was charged that many calls.
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
