module Tests.Optimization (optimizationTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, optimized)

optimizationTests :: TestTree
optimizationTests = testGroup "Optimization-based Integration Testing"
  [ 
    testContract "optimize/linear.sol"   (Just "optimize/config.yaml")
      [ ("Max value found in echidna_opt_linear",  optimized "echidna_opt_linear" 4000)
      ]
  ]
