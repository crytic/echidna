module Tests.Symbolic (symbolicTests) where

import Test.Tasty (TestTree, testGroup)
import Common (testContract', solved, verified)
import Echidna.Types.Campaign (WorkerType(..))

symbolicTests :: TestTree
symbolicTests = testGroup "Symbolic tests"
  [ testContract' "symbolic/verify.sol" Nothing Nothing (Just "symbolic/verify.yaml") True SymbolicWorker
      [ ("simple passed", solved "simple")
      , ("array passed", solved "array")
      , ("negative passed", solved "negative")
      , ("close passed", solved "close")
      , ("far not verified", verified "far")
      , ("correct not verified", verified "correct") 
    ]
  ]
