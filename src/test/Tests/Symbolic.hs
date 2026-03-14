module Tests.Symbolic (symbolicTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract', solcV, solved, passed, verified)
import Echidna.Types.Worker (WorkerType(..))

symbolicTests :: TestTree
symbolicTests = testGroup "Symbolic tests" $
  map (\conf ->
    testContract' "symbolic/verify.sol" (Just "VulnerableContract") (Just (>= solcV (0,6,9))) (Just conf) True SymbolicWorker
      [ ("simple passed", solved "simple")
      , ("array passed", solved "array")
      , ("negative passed", solved "negative")
      , ("close passed", solved "close")
      , ("far not verified", verified "far")
      , ("correct not verified", verified "correct")
      ]
  ) ["symbolic/verify.yaml", "symbolic/verify.bitwuzla.yaml"]
  ++ [
    testContract' "symbolic/verify-property.sol" (Just "PropertyVerifyTest") (Just (>= solcV (0,6,9))) (Just "symbolic/verify-property.yaml") True SymbolicWorker
      [ ("echidna_x_is_not_ten falsified", solved "echidna_x_is_not_ten")
      , ("echidna_x_is_not_one passed", passed "echidna_x_is_not_one")
      , ("echidna_y_below_40 falsified", solved "echidna_y_below_40")
      , ("echidna_locked_before_1000 falsified", solved "echidna_locked_before_1000")
      ]
  ]
  ++ ([
    -- This test is commented out because it requires a specific setup where both the FuzzWorker and SymbolicWorker are used.
    -- If you run the symbolic worker alone, it will hang indefinitely.
    --, testContract' "symbolic/explore.sol" Nothing Nothing (Just "symbolic/explore.yaml") True SymbolicWorker
    --  [ ("f passed", solved "f")
    --]
  ] :: [TestTree])
