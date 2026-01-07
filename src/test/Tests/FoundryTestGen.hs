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
    Nothing -> assertFailure "forge not found"
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

-- | Check if a test is a failed assertion.
isFailedAssertion :: EchidnaTest -> Bool
isFailedAssertion test = case test.testType of
  AssertionTest {} -> case test.state of
    Large _ -> True
    _       -> False
  _ -> False


-- ============================================================================
-- Tests
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
  let failedTests = filter isFailedAssertion tests
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
