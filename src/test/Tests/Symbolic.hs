module Tests.Symbolic (symbolicTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract', solcV, solved, verified)
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
  ++ ([
    -- dynamic (bytes) calldata: hevm concretizes it up to maxDynSize (128 bytes),
    -- so the symbolic worker can solve it. Run under bitwuzla only.
    testContract' "symbolic/verify.sol" (Just "VulnerableContract") (Just (>= solcV (0,6,9))) (Just "symbolic/verify.bitwuzla.yaml") True SymbolicWorker
      [ ("dynamic passed", solved "dynamic") ]
    -- This test is commented out because it requires a specific setup where both the FuzzWorker and SymbolicWorker are used.
    -- If you run the symbolic worker alone, it will hang indefinitely.
    --, testContract' "symbolic/explore.sol" Nothing Nothing (Just "symbolic/explore.yaml") True SymbolicWorker
    --  [ ("f passed", solved "f")
    --]
  ] :: [TestTree])
