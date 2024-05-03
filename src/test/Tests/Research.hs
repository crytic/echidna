module Tests.Research (researchTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContract', solcV, solved)

import Echidna.Types.Campaign (WorkerType(..))

researchTests :: TestTree
researchTests = testGroup "Research-based Integration Testing"
  [ testContract "research/harvey_foo.sol" Nothing
      [ ("echidna_assert failed",     solved "echidna_assert") ]
  , testContract' "research/harvey_baz.sol" Nothing Nothing Nothing False FuzzWorker
      [ ("echidna_all_states failed", solved "echidna_all_states") ]
  , testContract' "research/ilf_crowdsale.sol" Nothing (Just (\v -> v >= solcV (0,5,0) && v < solcV (0,6,0))) (Just "research/ilf_crowdsale.yaml") False FuzzWorker
      [ ("echidna_assert failed", solved "withdraw") ]
  , testContract' "research/solcfuzz_funwithnumbers.sol" (Just "VerifyFunWithNumbers") (Just (< solcV (0,6,0))) (Just "research/solcfuzz_funwithnumbers.yaml") True FuzzWorker
      [ ("echidna_assert failed", solved "sellTokens"),
        ("echidna_assert failed", solved "buyTokens")
      ]
  ]
