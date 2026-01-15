module Tests.FoundryTestGen (foundryTestGenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Control.Exception (catch, SomeException)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Text (pack, unpack, replace)
import qualified Data.Text.Lazy as TL
import System.Directory (getTemporaryDirectory, removePathForcibly, findExecutable, copyFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Common (testContract, testContract', solved)
import EVM.ABI (AbiValue(..))
import Echidna.Output.Foundry (foundryTest)
import Echidna.Types.Test (EchidnaTest(..), TestType(..), TestValue(..), TestState(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.Types.Worker (WorkerType(FuzzWorker, SymbolicWorker))

foundryTestGenTests :: TestTree
foundryTestGenTests = testGroup "Foundry test generation"
  [ testCase "compiles with forge" testForgeCompilation
  , testCase "correctly encodes bytes1" testBytes1Encoding
  , testCase "fallback function syntax" testFallbackSyntax
  , testCase "null bytes in arguments" testNullBytes
  , testGroup "Concrete execution (fuzzing)"
      [ testGroup "assertTrue"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertTrueTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_true")
              ]
          ]
      , testGroup "assertFalse"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertFalseTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_false")
              ]
          ]
      , testGroup "assertEq"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertEqTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_eq")
              ]
          ]
      , testGroup "assertNotEq"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertNotEqTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_not_eq")
              ]
          ]
      , testGroup "assertEqDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertEqDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_eq_decimal")
              ]
          ]
      , testGroup "assertNotEqDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertNotEqDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_not_eq_decimal")
              ]
          ]
      , testGroup "assertLt"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertLtTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_lt")
              ]
          ]
      , testGroup "assertGt"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertGtTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_gt")
              ]
          ]
      , testGroup "assertLtDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertLtDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_lt_decimal")
              ]
          ]
      , testGroup "assertGtDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertGtDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_gt_decimal")
              ]
          ]
      , testGroup "assertLe"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertLeTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_le")
              ]
          ]
      , testGroup "assertGe"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertGeTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_ge")
              ]
          ]
      , testGroup "assertLeDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertLeDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_le_decimal")
              ]
          ]
      , testGroup "assertGeDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertGeDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_ge_decimal")
              ]
          ]
      , testGroup "assertApproxEqAbs"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertApproxEqAbsTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_approx_eq_abs")
              ]
          ]
      , testGroup "assertApproxEqAbsDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertApproxEqAbsDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_approx_eq_abs_decimal")
              ]
          ]
      , testGroup "assertApproxEqRel"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertApproxEqRelTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_approx_eq_rel")
              ]
          ]
      , testGroup "assertApproxEqRelDecimal"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertApproxEqRelDecimalTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected", solved "test_assert_approx_eq_rel_decimal")
              ]
          ]
      , testGroup "stateless bug"
          [ testContract "foundry/StatelessBug.sol" (Just "foundry/StatelessBug.yaml")
              [ ("should be detected", solved "checkValue")
              ]
          ]
      , testGroup "revert"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "RevertTest") Nothing (Just "foundry/FoundryAsserts.yaml")
              True FuzzWorker
              [ ("should be detected as failure", solved "test_revert_is_failure")
              ]
          ]
      ]
  , testGroup "Symbolic execution (SMT solving)"
      [ testGroup "assertTrue"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertTrueTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_true")
              ]
          ]
      , testGroup "assertFalse"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertFalseTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_false")
              ]
          ]
      , testGroup "assertEq"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertEqTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_eq")
              ]
          ]
      , testGroup "assertNotEq"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertNotEqTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_not_eq")
              ]
          ]
      , testGroup "assertLt"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertLtTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_lt")
              ]
          ]
      , testGroup "assertGt"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertGtTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_gt")
              ]
          ]
      , testGroup "assertLe"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertLeTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_le")
              ]
          ]
      , testGroup "assertGe"
          [ testContract' "foundry/FoundryAsserts.sol"
              (Just "AssertGeTest") Nothing (Just "foundry/FoundryAssertsSymbolic.yaml")
              True SymbolicWorker
              [ ("should be detected", solved "test_assert_ge")
              ]
          ]
      -- Note: The following assertions are NOT supported in symbolic execution
      -- mode because hevm's symbolic execution engine doesn't recognize the
      -- following cheatcodes:
      -- - assertEqDecimal, assertNotEqDecimal
      -- - assertLtDecimal, assertGtDecimal, assertLeDecimal, assertGeDecimal
      -- - assertApproxEqAbs, assertApproxEqAbsDecimal
      -- - assertApproxEqRel, assertApproxEqRelDecimal
      -- These are only tested in concrete (fuzzing) mode above.
      ]
  ]

-- | Verify generated test compiles with forge.
testForgeCompilation :: IO ()
testForgeCompilation =
  testForgeCompiles "forge-compilation-test" "FoundryTestTarget" mkMinimalTest "Generated.t.sol"

-- | Test that generated test with fallback function call compiles with forge.
testFallbackSyntax :: IO ()
testFallbackSyntax =
  let fallbackTest = mkMinimalTest
        { reproducer = [Tx (SolCall ("", [])) 0 0 0 0 0 (0, 0)] }
  in testForgeCompiles "forge-fallback-test" "FallbackTest" fallbackTest "FallbackGenerated.t.sol"

-- | Test that generated test with null bytes in arguments compiles with forge.
testNullBytes :: IO ()
testNullBytes =
  let nullByteData = BS.pack [0x00, 0x01, 0x00, 0x02, 0x00, 0x00, 0x03]  -- Mix of null and non-null bytes
      nullByteArg = AbiBytes 32 (nullByteData <> BS.replicate (32 - BS.length nullByteData) 0)
      nullByteTest = mkMinimalTest
        { reproducer = [Tx (SolCall ("checkBytes", [nullByteArg])) 0 0 0 0 0 (0, 0)] }
  in testForgeCompiles "forge-nullbyte-test" "NullByteTest" nullByteTest "NullByteGenerated.t.sol"

-- | Helper function to test that generated Foundry code compiles with forge.
-- Takes a test description, contract name, test data, and output file name.
-- We use temp directories because we need to test the full forge workflow:
-- forge init (for dependencies) + our generated test + forge build.
testForgeCompiles :: String -> String -> EchidnaTest -> String -> IO ()
testForgeCompiles tmpDirSuffix contractName testData outputFile = do
  forgeExe <- findExecutable "forge"
  case forgeExe of
    Nothing ->
      assertFailure "forge not found"
    Just _ -> do
      tmpBase <- getTemporaryDirectory
      let tmpDir = tmpBase ++ "/echidna-" ++ tmpDirSuffix
          contractFile = contractName ++ ".sol"
          contractPath = "foundry/" ++ contractFile

      catch (removePathForcibly tmpDir) (\(_ :: SomeException) -> pure ())

      -- Initialize project with forge.
      (code, _, err) <- readProcessWithExitCode "forge" ["init", tmpDir] ""
      if code /= ExitSuccess
        then assertFailure $ "forge init failed: " ++ err
        else do
          copyFile contractPath (tmpDir ++ "/src/" ++ contractFile)

          -- Generate test and add contract import after forge-std import
          let generated = TL.unpack $ foundryTest (Just (pack contractName)) testData
              forgeStdImport = pack "import \"forge-std/Test.sol\";"
              contractImport = pack $ "import \"../src/" ++ contractFile ++ "\";"
              testWithImport = unpack $ replace forgeStdImport
                                               (forgeStdImport <> "\n" <> contractImport)
                                               (pack generated)

          writeFile (tmpDir ++ "/test/" ++ outputFile) testWithImport

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
