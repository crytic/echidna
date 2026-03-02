module Tests.Foundry (foundryTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract', solcV, solved, passed)
import Echidna.Types.Worker (WorkerType(..))

foundryTests :: TestTree
foundryTests = testGroup "Foundry Integration Testing"
  [ testContract' "dapptest/basic.sol" (Just "GreeterTest") (Just (\v -> v >= solcV (0,7,5))) (Just "dapptest/config.yaml") False FuzzWorker
     [
        ("testShrinking passed", solved "testShrinking"),
        ("testFuzzFixedArray passed", solved "testFuzzFixedArray"),
        ("testFuzzVariableArray passed", solved "testFuzzVariableArray"),
        ("testFuzzBytes1 passed", solved "testFuzzBytes1"),
        ("testFuzzBytes14 passed", solved "testFuzzBytes14"),
        ("testFuzzBytes32 passed", solved "testFuzzBytes32"),
        ("testFuzzI256 passed", solved "testFuzzI256"),
        ("testFuzzAbiCoderV2 passed", solved "testFuzzAbiCoderV2"),
        ("testAssume failed", passed "testFuzzAssume")
      ]
  ]
