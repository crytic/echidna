module Tests.FoundryTestGen (foundryTestGenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Control.Exception (catch, SomeException)
import Data.List (isInfixOf)
import Data.Text (pack, unpack, replace)
import qualified Data.Text.Lazy as TL
import System.Directory (getTemporaryDirectory, removePathForcibly, findExecutable, copyFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import EVM.ABI (AbiValue(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Output.Foundry (foundryTest)
import Echidna.Types.Test (EchidnaTest(..), TestType(..), TestValue(..), TestState(..))

foundryTestGenTests :: TestTree
foundryTestGenTests = testGroup "Foundry test generation"
  [ testCase "compiles with forge" testForgeCompilation
  , testCase "correctly encodes bytes1" testBytes1Encoding
  ]

-- | Verify generated test compiles with forge.
-- We use temp directories because we need to test the full forge workflow:
-- forge init (for dependencies) + our generated test + forge build.
testForgeCompilation :: IO ()
testForgeCompilation = do
  forgeExe <- findExecutable "forge"
  case forgeExe of
    Nothing -> 
      assertFailure "forge not found"
    Just _ -> do
      tmpBase <- getTemporaryDirectory
      let tmpDir = tmpBase ++ "/echidna-forge-test"
      
      catch (removePathForcibly tmpDir) (\(_ :: SomeException) -> pure ())
      
      -- Initialize project with forge.
      (code, _, err) <- readProcessWithExitCode "forge" ["init", tmpDir] ""
      if code /= ExitSuccess
        then assertFailure $ "forge init failed: " ++ err
        else do
          copyFile "foundry/FoundryTestTarget.sol" (tmpDir ++ "/src/FoundryTestTarget.sol")
          
          -- Simulate user action: Replace the target contract with the actual
          -- contract instance and import it (add contract import after the
          -- forge-std one).
          let generated = TL.unpack $ foundryTest (Just "FoundryTestTarget") mkMinimalTest
              forgeStdImport = pack "import \"forge-std/Test.sol\";"
              contractImport = pack "import \"../src/FoundryTestTarget.sol\";"
              testWithImport = unpack $ replace forgeStdImport 
                                               (forgeStdImport <> "\n" <> contractImport) 
                                               (pack generated)
          
          writeFile (tmpDir ++ "/test/Generated.t.sol") testWithImport
          
          (buildCode, _, buildErr) <- readProcessWithExitCode "forge" ["build", "--root", tmpDir] ""
          
          catch (removePathForcibly tmpDir) (\(_ :: SomeException) -> pure ())
          
          if buildCode == ExitSuccess
            then pure ()
            else assertFailure $ "forge build failed: " ++ buildErr

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

testBytes1Encoding :: IO ()
testBytes1Encoding = do
  let
    -- This reproducer failed to be encoded as a string in the past.
    reproducerTx = Tx
      { call = SolCall ("f", [AbiBytes 1 "\x92"])
      , src = 0
      , dst = 0
      , value = 0
      , gas = 0
      , gasprice = 0
      , delay = (0, 0)
      }
    test = mkMinimalTest { reproducer = [reproducerTx] }
    generated = TL.unpack $ foundryTest (Just "FoundryTestTarget") test
  if "hex\"92\"" `isInfixOf` generated
    then pure ()
    else assertFailure $ "bytes1 not correctly encoded: " ++ generated
