module Tests.Stateless (statelessTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContractV, testContract', solcV, solved, passed)

statelessTests :: TestTree
statelessTests = testGroup "Stateless Integration Testing"
  [ testContract' "stateless/basic.sol" (Just "GreeterTest") (Just (\v -> v >= solcV (0,7,5))) (Just "stateless/config.yaml") False
     [ 
        ("testShrinking passed", solved "testShrinking"),
        ("testFuzzFixedArray passed", solved "testFuzzFixedArray"),
        ("testFuzzVariableArray passed", solved "testFuzzVariableArray"),
        ("testFuzzVariableArray passed", solved "testFuzzVariableArray"),
        ("testFuzzBytes1 passed", solved "testFuzzBytes1"),
        ("testFuzzBytes14 passed", solved "testFuzzBytes14"),
        ("testFuzzBytes32 passed", solved "testFuzzBytes32"),
        ("testFuzzI256 passed", solved "testFuzzI256"),
        ("testFuzzAbiCoderV2 passed", solved "testFuzzAbiCoderV2"), 
        ("testAssume failed", passed "testFuzzAssume")
      ]
  ]
