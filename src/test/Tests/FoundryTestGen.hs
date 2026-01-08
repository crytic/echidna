module Tests.FoundryTestGen (foundryTestGenTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)

import Control.Exception (catch, SomeException)
import Data.IORef (readIORef)
import Data.List (isInfixOf)
import Data.Text (pack, unpack, replace)
import qualified Data.Text.Lazy as TL
import System.Directory (getTemporaryDirectory, removePathForcibly, findExecutable, copyFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import Common (runContract)
import Echidna.Config (parseConfig)
import Echidna.Output.Foundry (foundryTest)
import Echidna.Types.Config (Env(..), EConfigWithUsage(..))
import Echidna.Types.Test (EchidnaTest(..), TestType(..), TestValue(..), TestState(..), didFail, isAssertionTest)
import Echidna.Types.Worker (WorkerType(..))

foundryTestGenTests :: TestTree
foundryTestGenTests = testGroup "Foundry test generation"
  [ testCase "compiles with forge" testForgeCompilation
  , testCase "stateless bug reproduction" testStatelessBug
  , testGroup "Concrete execution (fuzzing)"
      [ testCase "concrete foundry assertTrue detection" testFoundryAssertTrueConcrete
      , testCase "concrete foundry assertFalse detection" testFoundryAssertFalseConcrete
      , testCase "concrete foundry assertEq detection" testFoundryAssertEqConcrete
      , testCase "concrete foundry assertNotEq detection" testFoundryAssertNotEqConcrete
      ]
  , testGroup "Symbolic execution (SMT solving)"
      [ testCase "symbolic foundry assertTrue detection" testFoundryAssertTrueSymbolic
      , testCase "symbolic foundry assertFalse detection" testFoundryAssertFalseSymbolic
      , testCase "symbolic foundry assertEq detection" testFoundryAssertEqSymbolic
      , testCase "symbolic foundry assertNotEq detection" testFoundryAssertNotEqSymbolic
      ]
  ]

-- ============================================================================
-- Test helpers
-- ============================================================================

-- General utilities

-- | Run an IO action that requires forge, failing if forge is not found.
requireForge :: IO () -> IO ()
requireForge action = do
  forgeExe <- findExecutable "forge"
  case forgeExe of
    Nothing -> assertFailure "forge not found. Install from https://getfoundry.sh"
    Just _ -> action

-- | Run an IO action that requires z3, failing if z3 is not found.
requireZ3 :: IO () -> IO ()
requireZ3 action = do
  z3Exe <- findExecutable "z3"
  case z3Exe of
    Nothing -> assertFailure "z3 not found. Install from https://github.com/Z3Prover/z3/releases"
    Just _ -> action

-- | Run an IO action with a temporary directory, cleaning up before and after.
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
  tmpBase <- getTemporaryDirectory
  let tmpDir = tmpBase </> name
  
  -- Clean up any existing directory.
  catch (removePathForcibly tmpDir) (\(_ :: SomeException) -> pure ())
  
  -- Run the action.
  result <- action tmpDir
  
  -- Cleanup.
  catch (removePathForcibly tmpDir) (\(_ :: SomeException) -> pure ())
  
  pure result

-- Foundry-specific utilities

-- | Add contract import to generated Foundry test.
-- Inserts the contract import right after the forge-std import.
addContractImport :: String -> TL.Text -> String
addContractImport contractName generatedTest =
  let forgeStdImport = pack "import \"forge-std/Test.sol\";"
      contractImport = pack $ "import \"../src/" ++ contractName ++ ".sol\";"
      testWithImport = replace forgeStdImport 
                               (forgeStdImport <> "\n" <> contractImport) 
                               (pack $ TL.unpack generatedTest)
  in unpack testWithImport

-- Test fixtures

-- | Create a minimal test for Foundry test generation.
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

-- | Helper to test a Foundry assertX function with a specific execution
-- strategy.
-- Runs Echidna on a contract, generates a Foundry test, and verifies forge
-- reproduces the assertion failure.
testFoundryAssert :: String -> String -> WorkerType -> String -> IO ()
testFoundryAssert contractName assertType workerType configFile = requireForge $ do
  -- Load config from YAML.
  parsed <- parseConfig ("foundry" </> configFile)
  let cfg = parsed.econfig
  
  -- Run Echidna to find the assertion failure.
  (env, _) <- runContract ("foundry" </> "FoundryAsserts.sol") 
                          (Just $ pack contractName) 
                          cfg
                          workerType
  
  -- Get test results.
  tests <- traverse readIORef env.testRefs
  
  -- Find the failed assertion test.
  let failedTests = filter (\t -> isAssertionTest t && didFail t) tests
      workerDesc = case workerType of
        SymbolicWorker -> "Symbolic execution"
        _ -> "Echidna"
  case failedTests of
    [] -> assertFailure $ workerDesc ++ " should find " ++ assertType ++ " failure in " ++ contractName
    (failedTest:_) -> do
      -- Verify we have a reproducer.
      assertBool "Failed test should have reproducer" 
        (not $ null failedTest.reproducer)
      
      -- Generate Foundry test.
      let generatedTest = foundryTest (Just $ pack contractName) failedTest
          testWithImport = addContractImport "FoundryAsserts" generatedTest
      
      -- Setup forge project and run test.
      let dirSuffix = case workerType of
            SymbolicWorker -> "symbolic-" ++ assertType
            FuzzWorker -> "concrete-" ++ assertType
      withTempDir ("echidna-forge-" ++ dirSuffix ++ "-test") $ \tmpDir -> do
        (code, _, err) <- readProcessWithExitCode "forge" ["init", tmpDir] ""
        if code /= ExitSuccess
          then assertFailure $ "forge init failed: " ++ err
          else do
            -- Copy contract and add imports to test.
            copyFile ("foundry" </> "FoundryAsserts.sol") (tmpDir </> "src" </> "FoundryAsserts.sol")
            
            writeFile (tmpDir </> "test" </> "Echidna.t.sol") testWithImport
          
            -- Build.
            (buildCode, _, buildErr) <- readProcessWithExitCode "forge" 
              ["build", "--root", tmpDir] ""
            
            if buildCode /= ExitSuccess
              then assertFailure $ "forge build failed: " ++ buildErr
              else do
                -- Run test - should FAIL (reproducing the assertion bug).
                (testCode, testOut, testErr) <- readProcessWithExitCode "forge" 
                  ["test", "--root", tmpDir, "--match-test", "test_replay"] ""
                
                -- Verify forge reproduced the assertion failure.
                assertBool ("forge test should fail (reproducing " ++ assertType ++ " failure)") 
                  (testCode /= ExitSuccess)
                
                let output = testOut ++ testErr
                assertBool "output should mention assertion or test failure" 
                  ("assert" `isInfixOf` output || "FAIL" `isInfixOf` output)

-- | Helper to test a Foundry assertX function using concrete execution
-- (fuzzing).
testFoundryAssertConcrete :: String -> String -> IO ()
testFoundryAssertConcrete contractName assertType = 
  testFoundryAssert contractName assertType FuzzWorker "FoundryAsserts.yaml"

-- | Helper to test a Foundry assertX function using symbolic execution (SMT
-- solving).
testFoundryAssertSymbolic :: String -> String -> IO ()
testFoundryAssertSymbolic contractName assertType = requireZ3 $ 
  testFoundryAssert contractName assertType SymbolicWorker "FoundryAssertsSymbolic.yaml"

-- ============================================================================
-- Tests
-- ============================================================================

-- ============================================================================
-- Concrete Tests
-- ============================================================================

-- | Verify generated test compiles with forge.
-- We use temp directories because we need to test the full forge workflow:
-- forge init (for dependencies) + our generated test + forge build.
testForgeCompilation :: IO ()
testForgeCompilation = requireForge $ withTempDir "echidna-forge-compilation-test" $ \tmpDir -> do
  -- Initialize project with forge.
  (code, _, err) <- readProcessWithExitCode "forge" ["init", tmpDir] ""
  if code /= ExitSuccess
    then assertFailure $ "forge init failed: " ++ err
    else do
      copyFile ("foundry" </> "FoundryTestTarget.sol") (tmpDir </> "src" </> "FoundryTestTarget.sol")
      
      -- Simulate user action: Replace the target contract with the actual
      -- contract instance and import it (add contract import after the
      -- forge-std one).
      let generated = foundryTest (Just "FoundryTestTarget") mkMinimalTest
          testWithImport = addContractImport "FoundryTestTarget" generated
      
      writeFile (tmpDir </> "test" </> "Generated.t.sol") testWithImport
      
      (buildCode, _, buildErr) <- readProcessWithExitCode "forge" ["build", "--root", tmpDir] ""
      
      if buildCode == ExitSuccess
        then pure ()
        else assertFailure $ "forge build failed: " ++ buildErr

-- | Run Echidna on a stateless contract with a bug, generate Foundry test,
-- and verify forge reproduces the bug.
testStatelessBug :: IO ()
testStatelessBug = requireForge $ do
  -- Load config from YAML (assertion mode with fixed seed for deterministic
  -- results). The bug is trivial and should always be found, but a fixed seed
  -- ensures the test is reproducible in CI.
  parsed <- parseConfig ("foundry" </> "StatelessBug.yaml")
  let cfg = parsed.econfig
  
  -- Run Echidna to find the bug.
  (env, _) <- runContract ("foundry" </> "StatelessBug.sol") 
                          (Just "StatelessBuggy") 
                          cfg
                          FuzzWorker
  
  -- Get test results.
  tests <- traverse readIORef env.testRefs
  
  -- Find the failed assertion test.
  let failedTests = filter (\t -> isAssertionTest t && didFail t) tests
  case failedTests of
    [] -> assertFailure "Echidna should find assertion failure in StatelessBug.sol"
    (failedTest:_) -> do
      -- Verify we have a reproducer.
      assertBool "Failed test should have reproducer" 
        (not $ null failedTest.reproducer)
      
      -- Generate Foundry test.
      let generatedTest = foundryTest (Just "StatelessBuggy") failedTest
          testWithImport = addContractImport "StatelessBuggy" generatedTest
      
      -- Setup forge project and run test.
      withTempDir "echidna-forge-stateless-bug-test" $ \tmpDir -> do
        (code, _, err) <- readProcessWithExitCode "forge" ["init", tmpDir] ""
        if code /= ExitSuccess
          then assertFailure $ "forge init failed: " ++ err
          else do
            -- Copy contract and add imports to test.
            copyFile ("foundry" </> "StatelessBug.sol") (tmpDir </> "src" </> "StatelessBuggy.sol")
            
            writeFile (tmpDir </> "test" </> "Generated.t.sol") testWithImport
          
            -- Build.
            (buildCode, _, buildErr) <- readProcessWithExitCode "forge" 
              ["build", "--root", tmpDir] ""
            
            if buildCode /= ExitSuccess
              then assertFailure $ "forge build failed: " ++ buildErr
              else do
                -- Run test - should FAIL (reproducing the bug).
                (testCode, testOut, testErr) <- readProcessWithExitCode "forge" 
                  ["test", "--root", tmpDir, "--match-test", "test_replay"] ""
                
                -- Verify forge reproduced the assertion failure.
                assertBool "forge test should fail (reproducing assertion bug)" 
                  (testCode /= ExitSuccess)
                
                let output = testOut ++ testErr
                assertBool "output should mention assertion failure" 
                  ("assert" `isInfixOf` output || "Panic" `isInfixOf` output)

-- | Test Foundry's assertTrue detection.
testFoundryAssertTrueConcrete :: IO ()
testFoundryAssertTrueConcrete = testFoundryAssertConcrete "AssertTrueTest" "assertTrue"

-- | Test Foundry's assertFalse detection.
testFoundryAssertFalseConcrete :: IO ()
testFoundryAssertFalseConcrete = testFoundryAssertConcrete "AssertFalseTest" "assertFalse"

-- | Test Foundry's assertEq detection.
testFoundryAssertEqConcrete :: IO ()
testFoundryAssertEqConcrete = testFoundryAssertConcrete "AssertEqTest" "assertEq"

-- | Test Foundry's assertNotEq detection.
testFoundryAssertNotEqConcrete :: IO ()
testFoundryAssertNotEqConcrete = testFoundryAssertConcrete "AssertNotEqTest" "assertNotEq"

-- ============================================================================
-- Symbolic Execution Tests
-- ============================================================================

-- | Test Foundry's assertTrue detection with symbolic execution.
-- Uses the same AssertTrueTest contract as concrete tests, but with symbolic execution.
testFoundryAssertTrueSymbolic :: IO ()
testFoundryAssertTrueSymbolic = testFoundryAssertSymbolic "AssertTrueTest" "assertTrue"

-- | Test Foundry's assertEq detection with symbolic execution.
-- Uses the same AssertEqTest contract as concrete tests, but with symbolic execution.
testFoundryAssertEqSymbolic :: IO ()
testFoundryAssertEqSymbolic = testFoundryAssertSymbolic "AssertEqTest" "assertEq"

-- | Test Foundry's assertFalse detection with symbolic execution.
-- Uses the same AssertFalseTest contract as concrete tests, but with symbolic execution.
testFoundryAssertFalseSymbolic :: IO ()
testFoundryAssertFalseSymbolic = testFoundryAssertSymbolic "AssertFalseTest" "assertFalse"

-- | Test Foundry's assertNotEq detection with symbolic execution.
-- Uses the same AssertNotEqTest contract as concrete tests, but with symbolic execution.
testFoundryAssertNotEqSymbolic :: IO ()
testFoundryAssertNotEqSymbolic = testFoundryAssertSymbolic "AssertNotEqTest" "assertNotEq"