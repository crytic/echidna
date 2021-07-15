module Tests.Assertion (assertionTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContractV, solcV, solved, solvedUsing, passed)

assertionTests :: TestTree
assertionTests = testGroup "Assertion-based Integration Testing"
  [ 
      testContractV "assert/assert.sol"  (Just (\v -> v < solcV (0,8,0)))  (Just "assert/config.yaml")
      [ ("direct_assert passed",           solved  "direct_assert")
      , ("internal_assert passed",         solved  "internal_assert")
      , ("f failed",                       passed  "f")
      ]

    , testContractV "assert/assert-0.8.sol" (Just (\v -> v >= solcV (0,8,0))) (Just "assert/config.yaml")
      [ ("direct_assert passed", solvedUsing "direct_assert" "Assertion failure detector") ] 

    , testContract "assert/revert.sol"   (Just "assert/config.yaml")
      [ ("assert_revert passed",       solvedUsing "assert_revert" "AssertionFailed(..)")
      , ("assert_unreachable failed",  passed      "assert_unreachable")
      ]

    , testContract "assert/external.sol" (Just "assert/config.yaml")
      [ ("assert_external passed",     solvedUsing "assert_external" "AssertionFailed(..)")
      , ("assert_call failed",         passed      "external_call")
      ]

    , testContract "assert/multi.sol" (Just "assert/multi.yaml")
      [ ("fail passed",     solvedUsing "fail" "AssertionFailed(..)")
      , ("f failed",         passed     "f")
      ]
  ]
