module Tests.Assertion (assertionTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContract', testContractV, solcV, solved, solvedUsing, passed)

import Echidna.Types.Campaign (WorkerType(..))

assertionTests :: TestTree
assertionTests = testGroup "Assertion-based Integration Testing"
  [
      testContractV "assert/assert.sol"  (Just (\v -> v < solcV (0,8,0)))  (Just "assert/config.yaml")
      [ ("direct_assert passed",           solved  "direct_assert")
      , ("internal_assert passed",         solved  "internal_assert")
      , ("f failed",                       passed  "f")
      ]

      , testContract "assert/assert.sol"  (Just "assert/whitelist_asserts.yaml")
      [ ("direct_assert passed",           solved "direct_assert")
      , ("internal_assert failed",         solved "internal_assert")]

    , testContractV "assert/assert-0.8.sol" (Just (\v -> v >= solcV (0,8,0))) (Just "assert/config.yaml")
      [ ("direct_assert passed", solved "direct_assert") ]

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
    , testContract' "assert/conf.sol" (Just "A") Nothing (Just "assert/multi.yaml") True FuzzWorker
      [ ("c failed", passed "c") ]

    , testContract' "assert/fullmath.sol" (Just "FullMathEchidnaTest") (Just (\v -> v == solcV (0,7,5))) (Just "assert/config.yaml") False FuzzWorker
      [ ("checkMulDivRoundingUp failed", solved "checkMulDivRoundingUp") ]

  ]
