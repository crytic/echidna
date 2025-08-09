module Tests.Symbolic (symbolicTests) where

import Test.Tasty (TestTree, testGroup)
import Common (testContract', solcV, solved, verified)
import Echidna.Types.Worker (WorkerType(..))

symbolicTests :: TestTree
symbolicTests = testGroup "Symbolic tests"
  [ testContract' "symbolic/verify.sol" Nothing (Just (>= solcV (0,8,0))) (Just "symbolic/verify.yaml") True SymbolicWorker
      [ ("simple passed", solved "simple")
      , ("array passed", solved "array")
      , ("negative passed", solved "negative")
      , ("close passed", solved "close")
      , ("far not verified", verified "far")
      , ("correct not verified", verified "correct") 
    ] 
    -- This test is commented out because it requires a specific setup where both the FuzzWorker and SymbolicWorker are used.
    -- If you run the symbolic worker alone, it will hang indefinitely.
    --, testContract' "symbolic/explore.sol" Nothing Nothing (Just "symbolic/explore.yaml") True SymbolicWorker
    --  [ ("f passed", solved "f")
    --]
  ]
