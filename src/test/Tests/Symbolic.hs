module Tests.Symbolic (symbolicTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, solved)

symbolicTests :: TestTree
symbolicTests = testGroup "Symbolic tests"
  [
    testContract "symbolic/sym.sol" (Just "symbolic/sym.yaml")
      [ ("func_one passed", solved "func_one")
      , ("func_two passed", solved "func_two") ]
  ]
