module Tests.Foundry (foundryTests) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import EVM.ABI (AbiType(AbiUIntType))

import Common (testContract', solcV, solved, passed)
import Echidna.Solidity (filterMethodsWithArgs)
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Worker (WorkerType(..))

foundryTests :: TestTree
foundryTests = testGroup "Foundry Integration Testing"
  [ testGroup "ABI filtering"
      [ testCase "stateless fuzzing only calls parameterized test functions" $
          assertEqual "filtered signatures"
            ["testFuzz"]
            (fst <$> NE.toList (filterMethodsWithArgs 1 foundrySignatures))
      , testCase "invariant fuzzing retains the callable function set" $
          assertEqual "filtered signatures"
            ["testUnit", "testFuzz", "handler", "invariant_total"]
            (fst <$> NE.toList (filterMethodsWithArgs 100 foundrySignatures))
      ]
  , testContract' "foundry-basic/basic.sol" (Just "GreeterTest") (Just (\v -> v >= solcV (0,7,5))) (Just "foundry-basic/config.yaml") False FuzzWorker
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

foundrySignatures :: NonEmpty SolSignature
foundrySignatures =
  ("", []) :|
    [ ("setUp", [])
    , ("testUnit", [])
    , ("testFuzz", [AbiUIntType 256])
    , ("handler", [AbiUIntType 256])
    , ("invariant_total", [])
    ]
