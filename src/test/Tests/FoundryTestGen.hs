module Tests.FoundryTestGen (foundryTestGenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Data.Text (isInfixOf)
import qualified Data.Text.Lazy as TL

import Echidna.Output.Foundry (foundryTest)
import Echidna.Types.Test (EchidnaTest(..), TestType(..), TestValue(..), TestState(..))

foundryTestGenTests :: TestTree
foundryTestGenTests = testGroup "Foundry test generation tests"
  [ testCase "generates FoundryTest contract name" $ do
      let test = mkMinimalTest
          output = TL.toStrict $ foundryTest (Just "MyContract") test
      -- Verify the contract name avoids collision with `forge-std/Test.sol`.
      assertBool "output should contain 'contract FoundryTest is Test'" $
        "contract FoundryTest is Test" `isInfixOf` output
  ]

-- | Create a minimal EchidnaTest for output testing.
mkMinimalTest :: EchidnaTest
mkMinimalTest = EchidnaTest
  -- Foundry tests are only generated for solved/large tests.
  { state = Large 0
  -- AssertionTest is required for Foundry test generation.
  , testType = AssertionTest False ("test", []) 0
  , value = BoolValue True
  -- Empty reproducer is sufficient for testing contract name generation.
  , reproducer = []
  -- These fields are not read by the output generator.
  , result = error "result not needed for Foundry output tests"
  , vm = Nothing
  , workerId = Nothing
  }
