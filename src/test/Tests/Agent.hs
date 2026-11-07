module Tests.Agent (agentTests) where

import Data.IORef (newIORef)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Echidna.Types.Agent (mkFuzzerAgent, stateRefOf)
import Echidna.Types.Campaign (initialWorkerState)

agentTests :: TestTree
agentTests = testGroup "Agent tests"
  [ testCase "fuzzer agent uses the supplied state reference" $ do
      stateRef <- newIORef initialWorkerState
      let agent = mkFuzzerAgent undefined undefined 0 stateRef [] 0
      assertBool "agent must publish through the supplied reference" $
        stateRefOf agent == stateRef
  ]
