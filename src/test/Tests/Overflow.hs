module Tests.Overflow (overflowTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContractV, solcV, solvedUsing)

overflowTests :: TestTree
overflowTests = testGroup "Overflow-based Integration Testing"
  [ 
    testContractV "overflow/overflow.sol" (Just (\v -> v >= solcV (0,8,0))) (Just "overflow/config.yaml")
      [ ("f passed", solvedUsing "f" "Integer (over/under)flow") ] 
  ]
