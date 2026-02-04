module Tests.PrefillCorpus (prefillCorpusTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, assertFailure)

import Data.List (find, isInfixOf)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (findExecutable)

import EVM.Types (Addr)

import Echidna.SourceAnalysis.FoundryTests
  ( extractFoundryTests
  , foundryTestsToCorpus
  , FoundryTestInfo(..)
  , FoundryTestSequence(..)
  )
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Config (defaultConfig)
import Echidna.Types.Config (EConfig(..))

prefillCorpusTests :: TestTree
prefillCorpusTests = testGroup "Prefill corpus from Foundry tests"
  [ testCase "extracts correct number of sequences" testExtractSequenceCount
  , testCase "extracts correct number of transactions" testExtractTransactionCount
  , testCase "filters view/pure functions" testFilterViewPure
  , testCase "extracts increment sequence" testIncrementSequence
  , testCase "extracts set_multiple_values with args" testSetMultipleValuesArgs
  , testCase "converts to corpus format" testCorpusConversion
  , testCase "uses custom sender address" testCustomSenderAddress
  , testCase "uses custom contract address" testCustomContractAddress
  ]

-- | Custom addresses for testing
customSenderAddr :: Addr
customSenderAddr = 0xdeadbeef

customContractAddr :: Addr
customContractAddr = 0xcafebabe

-- | Get the SolConf with prefillCorpus enabled and default addresses
testSolConf :: SolConf
testSolConf = defaultConfig.solConf { prefillCorpus = True, quiet = True }

-- | Get the SolConf with custom addresses
testSolConfWithCustomAddrs :: SolConf
testSolConfWithCustomAddrs = defaultConfig.solConf
  { prefillCorpus = True
  , quiet = True
  , sender = Set.singleton customSenderAddr
  , contractAddr = customContractAddr
  }

-- | Path to the test contract (relative to tests/solidity)
testContractPath :: FilePath
testContractPath = "foundry-prefill/src/Counter.sol"

-- | Target contract name
targetContract :: Maybe Text
targetContract = Just "Counter"

-- | Helper to run extraction and check for python/slither availability
runExtraction :: IO (Maybe FoundryTestInfo)
runExtraction = runExtractionWith testSolConf

-- | Helper to run extraction with a specific SolConf
runExtractionWith :: SolConf -> IO (Maybe FoundryTestInfo)
runExtractionWith solConf = do
  pythonExe <- findExecutable "python3"
  case pythonExe of
    Nothing -> pure Nothing
    Just _ -> Just <$> extractFoundryTests testContractPath solConf targetContract

-- | Test that we extract the expected number of sequences (7 test functions)
testExtractSequenceCount :: IO ()
testExtractSequenceCount = do
  result <- runExtraction
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> assertEqual "Expected 7 test sequences" 7 (length info.sequences)

-- | Test that we extract the expected total number of transactions
-- 3 + 3 + 3 + 3 + 4 + 1 + 4 = 21 (view/pure filtered)
testExtractTransactionCount :: IO ()
testExtractTransactionCount = do
  result <- runExtraction
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      let totalTxs = sum $ map (\s -> length s.transactions) info.sequences
      assertEqual "Expected 21 transactions (view/pure filtered)" 21 totalTxs

-- | Test that view/pure functions are filtered out
-- test_with_view_and_pure_calls should have 4 calls, not 6
testFilterViewPure :: IO ()
testFilterViewPure = do
  result <- runExtraction
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      let viewPureSeq = find (\s -> "test_with_view_and_pure_calls" `isInfixOf` s.source) info.sequences
      case viewPureSeq of
        Nothing -> assertFailure "Could not find test_with_view_and_pure_calls sequence"
        Just seq' -> do
          assertEqual "Expected 4 calls (view/pure filtered)" 4 (length seq'.transactions)
          -- Verify the calls are: increment, setValue, setValue, decrement
          let callNames = map getCallName seq'.transactions
          assertBool "Should not contain getCount (view)" $ "getCount" `notElem` callNames
          assertBool "Should not contain add (pure)" $ "add" `notElem` callNames

-- | Test that increment sequence is correctly extracted
testIncrementSequence :: IO ()
testIncrementSequence = do
  result <- runExtraction
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      let incSeq = find (\s -> "test_increment_sequence" `isInfixOf` s.source) info.sequences
      case incSeq of
        Nothing -> assertFailure "Could not find test_increment_sequence"
        Just seq' -> do
          assertEqual "Expected 3 increment calls" 3 (length seq'.transactions)
          let allIncrement = all ((== "increment") . getCallName) seq'.transactions
          assertBool "All calls should be increment" allIncrement

-- | Test that arguments are extracted (setMultiple has constant args)
testSetMultipleValuesArgs :: IO ()
testSetMultipleValuesArgs = do
  result <- runExtraction
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      let setMultiSeq = find (\s -> "test_set_multiple_values" `isInfixOf` s.source) info.sequences
      case setMultiSeq of
        Nothing -> assertFailure "Could not find test_set_multiple_values"
        Just seq' -> do
          assertEqual "Expected 3 calls" 3 (length seq'.transactions)
          -- First call should be setMultiple
          case seq'.transactions of
            (tx:_) -> assertEqual "First call should be setMultiple" "setMultiple" (getCallName tx)
            [] -> assertFailure "No transactions found"

-- | Test conversion to corpus format
testCorpusConversion :: IO ()
testCorpusConversion = do
  result <- runExtraction
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      let corpus = foundryTestsToCorpus info
      assertEqual "Corpus should have 7 entries" 7 (length corpus)
      -- Check that all entries have non-empty transaction lists
      assertBool "All corpus entries should have transactions" $
        not (any (null . snd) corpus)

-- | Helper to get the function name from a Tx
getCallName :: Tx -> String
getCallName tx = case tx.call of
  SolCall (name, _) -> T.unpack name
  _ -> "<unknown>"

-- | Test that custom sender address is used in extracted transactions
testCustomSenderAddress :: IO ()
testCustomSenderAddress = do
  result <- runExtractionWith testSolConfWithCustomAddrs
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      -- Get first transaction from first sequence
      case info.sequences of
        [] -> assertFailure "No sequences extracted"
        (seq':_) -> case seq'.transactions of
          [] -> assertFailure "No transactions in sequence"
          (tx:_) -> do
            assertEqual "Sender should be custom address"
              customSenderAddr tx.src

-- | Test that custom contract address is used in extracted transactions
testCustomContractAddress :: IO ()
testCustomContractAddress = do
  result <- runExtractionWith testSolConfWithCustomAddrs
  case result of
    Nothing -> assertBool "python3 not available, skipping" True
    Just info -> do
      -- Get first transaction from first sequence
      case info.sequences of
        [] -> assertFailure "No sequences extracted"
        (seq':_) -> case seq'.transactions of
          [] -> assertFailure "No transactions in sequence"
          (tx:_) -> do
            assertEqual "Destination should be custom contract address"
              customContractAddr tx.dst
