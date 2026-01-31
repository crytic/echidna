module Tests.FoundryTestGen (foundryTestGenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import Control.Exception (catch, SomeException)
import Data.ByteString qualified as BS
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (pack, unpack, replace)
import qualified Data.Text.Lazy as TL
import System.Directory (getTemporaryDirectory, removePathForcibly, findExecutable, copyFile)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import Common (solved, testContract, testContractNamed)
import Echidna.Types.Config (Env)
import Echidna.Types.Campaign (WorkerState)
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
      [ testForgeStd "solves assertTrue"
          "foundry/FoundryAsserts.sol"
          (Just "AssertTrueTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertTrue should be detected concrete", solved "test_assert_true")
          ]
      , testForgeStd "solves assertFalse"
          "foundry/FoundryAsserts.sol"
          (Just "AssertFalseTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertFalse should be detected concrete", solved "test_assert_false")
          ]
      , testForgeStd "solves assertEq"
          "foundry/FoundryAsserts.sol"
          (Just "AssertEqTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertEq should be detected concrete", solved "test_assert_eq")
          ]
      , testForgeStd "solves assertNotEq"
          "foundry/FoundryAsserts.sol"
          (Just "AssertNotEqTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertNotEq should be detected concrete", solved "test_assert_not_eq")
          ]
      , testForgeStd "solves assertEqDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertEqDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertEqDecimal should be detected concrete", solved "test_assert_eq_decimal")
          ]
      , testForgeStd "solves assertNotEqDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertNotEqDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertNotEqDecimal should be detected concrete", solved "test_assert_not_eq_decimal")
          ]
      , testForgeStd "solves assertLt"
          "foundry/FoundryAsserts.sol"
          (Just "AssertLtTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertLt should be detected concrete", solved "test_assert_lt")
          ]
      , testForgeStd "solves assertGt"
          "foundry/FoundryAsserts.sol"
          (Just "AssertGtTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertGt should be detected concrete", solved "test_assert_gt")
          ]
      , testForgeStd "solves assertLtDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertLtDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertLtDecimal should be detected concrete", solved "test_assert_lt_decimal")
          ]
      , testForgeStd "solves assertGtDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertGtDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertGtDecimal should be detected concrete", solved "test_assert_gt_decimal")
          ]
      , testForgeStd "solves assertLe"
          "foundry/FoundryAsserts.sol"
          (Just "AssertLeTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertLe should be detected concrete", solved "test_assert_le")
          ]
      , testForgeStd "solves assertGe"
          "foundry/FoundryAsserts.sol"
          (Just "AssertGeTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertGe should be detected concrete", solved "test_assert_ge")
          ]
      , testForgeStd "solves assertLeDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertLeDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertLeDecimal should be detected concrete", solved "test_assert_le_decimal")
          ]
      , testForgeStd "solves assertGeDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertGeDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertGeDecimal should be detected concrete", solved "test_assert_ge_decimal")
          ]
      , testForgeStd "solves assertApproxEqAbs"
          "foundry/FoundryAsserts.sol"
          (Just "AssertApproxEqAbsTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertApproxEqAbs should be detected concrete", solved "test_assert_approx_eq_abs")
          ]
      , testForgeStd "solves assertApproxEqAbsDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertApproxEqAbsDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertApproxEqAbsDecimal should be detected concrete", solved "test_assert_approx_eq_abs_decimal")
          ]
      , testForgeStd "solves assertApproxEqRel"
          "foundry/FoundryAsserts.sol"
          (Just "AssertApproxEqRelTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertApproxEqRel should be detected concrete", solved "test_assert_approx_eq_rel")
          ]
      , testForgeStd "solves assertApproxEqRelDecimal"
          "foundry/FoundryAsserts.sol"
          (Just "AssertApproxEqRelDecimalTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("assertApproxEqRelDecimal should be detected concrete", solved "test_assert_approx_eq_rel_decimal")
          ]
      , testContract "foundry/StatelessBug.sol" (Just "foundry/StatelessBug.yaml")
          [ ("stateless bug should be detected concrete", solved "checkValue")
          ]
      , testForgeStd "solves revert"
          "foundry/FoundryAsserts.sol"
          (Just "RevertTest") (Just "foundry/FoundryAsserts.yaml")
          FuzzWorker
          [ ("revert should be detected as failure concrete", solved "test_revert_is_failure")
          ]
      , testForgeStd "solves invariant"
          "foundry/FoundryAsserts.sol"
          (Just "InvariantTest") (Just "foundry/FoundryInvariant.yaml")
          FuzzWorker
          [ ("invariant should be detected with seqLen > 1", solved "invariant_counter_below_limit")
          ]
      ]
  , testGroup "Symbolic execution (SMT solving)"
      [ testForgeStd "solves assertTrue"
          "foundry/FoundryAsserts.sol"
          (Just "AssertTrueTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertTrue should be detected symbolic", solved "test_assert_true")
          ]
      , testForgeStd "solves assertFalse"
          "foundry/FoundryAsserts.sol"
          (Just "AssertFalseTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertFalse should be detected symbolic", solved "test_assert_false")
          ]
      , testForgeStd "solves assertEq"
          "foundry/FoundryAsserts.sol"
          (Just "AssertEqTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertEq should be detected symbolic", solved "test_assert_eq")
          ]
      , testForgeStd "solves assertNotEq"
          "foundry/FoundryAsserts.sol"
          (Just "AssertNotEqTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertNotEq should be detected symbolic", solved "test_assert_not_eq")
          ]
      , testForgeStd "solves assertLt"
          "foundry/FoundryAsserts.sol"
          (Just "AssertLtTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertLt should be detected symbolic", solved "test_assert_lt")
          ]
      , testForgeStd "solves assertGt"
          "foundry/FoundryAsserts.sol"
          (Just "AssertGtTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertGt should be detected symbolic", solved "test_assert_gt")
          ]
      , testForgeStd "solves assertLe"
          "foundry/FoundryAsserts.sol"
          (Just "AssertLeTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertLe should be detected symbolic", solved "test_assert_le")
          ]
      , testForgeStd "solves assertGe"
          "foundry/FoundryAsserts.sol"
          (Just "AssertGeTest") (Just "foundry/FoundryAssertsSymbolic.yaml")
          SymbolicWorker
          [ ("assertGe should be detected symbolic", solved "test_assert_ge")
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

-- | Wrapper for testContractNamed that skips if solc < 0.8.13.
testForgeStd :: String -> FilePath -> Maybe String -> Maybe FilePath -> WorkerType -> [(String, (Env, WorkerState) -> IO Bool)] -> TestTree
testForgeStd name fp contract config workerType checks =
  if solcSupportsForgeStd
    then testContractNamed name fp (pack <$> contract) Nothing config True workerType checks
    else testCase name $ assertBool "skip (solc < 0.8.13, forge-std requires >= 0.8.13)" True

-- | Check if solc >= 0.8.13 (required for forge-std). Computed once on module
-- load. This is used to skip tests that require forge-std if solc is too old.
{-# NOINLINE solcSupportsForgeStd #-}
solcSupportsForgeStd :: Bool
solcSupportsForgeStd = unsafePerformIO $ do
  result <- findExecutable "solc"
  case result of
    Nothing -> pure False
    Just _ -> do
      (code, out, _) <- readProcessWithExitCode "solc" ["--version"] ""
      pure $ code == ExitSuccess && maybe False (>= (0, 8, 13)) (parseSolcVersion out)
  where
    parseSolcVersion :: String -> Maybe (Int, Int, Int)
    parseSolcVersion output =
      case filter ("Version:" `isPrefixOf`) (lines output) of
        (line:_) ->
          let versionPart = dropWhile (/= ':') line
              version = takeWhile (/= '+') $ drop 2 versionPart
              parts = words version
          in case parts of
               (v:_) -> parseVersion v
               _ -> Nothing
        _ -> Nothing

    parseVersion :: String -> Maybe (Int, Int, Int)
    parseVersion v = case map readMaybe (splitOn '.' v) of
      [Just major, Just minor, Just patch] -> Just (major, minor, patch)
      _ -> Nothing

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
      (a, _:b) -> a : splitOn c b
      (a, [])  -> [a]

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