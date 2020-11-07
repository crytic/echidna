module Tests.Research (researchTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, solved)

researchTests :: TestTree
researchTests = testGroup "Research-based Integration Testing"
  [ testContract "research/harvey_foo.sol" Nothing
      [ ("echidna_assert failed",     solved "echidna_assert") ]
  , testContract "research/harvey_baz.sol" Nothing
      [ ("echidna_all_states failed", solved "echidna_all_states") ]
  , testContract "research/ilf_crowsale.sol" (Just "research/ilf_crowsale.yaml")
      [ ("echidna_assert failed", solved "ASSERTION withdraw") ]

  ]
