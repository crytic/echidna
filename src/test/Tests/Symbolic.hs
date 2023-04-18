module Tests.Symbolic (symbolicTests) where

import System.Info (os)
import Test.Tasty (TestTree, testGroup)
import Common (testContract', solved, passed)
import Echidna.Types.Campaign (WorkerType(..))

symbolicTests :: TestTree
symbolicTests = testGroup "Symbolic tests" $ if os /= "linux" then [] else
  [ testContract' "symbolic/sym.sol" Nothing Nothing (Just "symbolic/sym.yaml") True SymbolicWorker
      [ ("echidna_sym passed", passed "echidna_sym") ]

  , testContract' "symbolic/sym-assert.sol" Nothing Nothing (Just "symbolic/sym-assert.yaml") True SymbolicWorker
      [ ("func_one passed", solved "func_one")
      , ("func_two passed", solved "func_two") ]
  ]
