module Tests.ABIv2 (abiv2Tests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, solved)

abiv2Tests :: TestTree
abiv2Tests = testGroup "ABIv2 tests"
  [ 
    testContract "abiv2/Ballot.sol"       Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/Dynamic.sol"      Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/Dynamic2.sol"     Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/MultiTuple.sol"   Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]

  ]
