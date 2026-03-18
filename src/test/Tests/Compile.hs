module Tests.Compile (compilationTests) where

import Control.Monad (void)
import Control.Monad.Catch (catch)
import Data.SemVer qualified
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Common (testConfig, solcV, withSolcVersion, loadSolTests)
import Echidna.Solidity (compileContracts)
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..), SolException(..))

compilationTests :: TestTree
compilationTests = testGroup "Compilation and loading tests"
  [ loadFails "bad/nocontract.sol" (Just "c") "failed to warn on contract not found" $
      \case ContractNotFound _ -> True; _ -> False
  , loadFailsV (>= solcV (0,6,0)) "bad/nobytecode.sol" Nothing "failed to warn on abstract contract" $
      \case NoBytecode _ -> True; _ -> False
  , loadFails "bad/nofuncs.sol"    Nothing    "failed to warn on no functions found" $
      \case NoFuncs -> True; _ -> False
  , loadFails "bad/notests.sol"    Nothing    "failed to warn on no tests found" $
      \case NoTests -> True; _ -> False
  , loadFails "bad/testargs.sol"   Nothing    "failed to warn on test args found" $
      \case TestArgsFound _ -> True; _ -> False
  , loadFails "bad/consargs.sol"   Nothing    "failed to warn on cons args found" $
      \case ConstructorArgs _ -> True; _ -> False
  , loadFails "bad/precompile.sol"  Nothing   "failed to warn on a failed deployment" $
      \case DeploymentFailed _ _ -> True; _ -> False
  , loadFails "bad/revert.sol"     Nothing    "failed to warn on a failed deployment" $
      \case DeploymentFailed _ _ -> True; _ -> False
  , loadFails "basic/eip-170.sol"  Nothing    "failed to warn on a failed deployment" $
      \case DeploymentFailed _ _ -> True; _ -> False
  , loadFailsWith "bad/propaliased.sol" Nothing "property" "failed to warn on property without bool return" $
      \case PropertyWithoutReturn _ -> True; _ -> False
  , loadFailsWith "bad/optaliased.sol" Nothing "optimization" "failed to warn on optimization with wrong return" $
      \case OptimizationWithWrongReturn _ -> True; _ -> False
  ]

loadFails :: FilePath -> Maybe Text -> String -> (SolException -> Bool) -> TestTree
loadFails fp c = loadFailsWith fp c "property"

loadFailsV :: (Data.SemVer.Version -> Bool) -> FilePath -> Maybe Text -> String -> (SolException -> Bool) -> TestTree
loadFailsV v fp c e p = testCase fp $ withSolcVersion (Just v) $
  catch tryLoad (assertBool e . p) where
  tryLoad = do
    let cfg = testConfig
    buildOutput <- compileContracts cfg.solConf (pure fp)
    void $ loadSolTests cfg buildOutput c

loadFailsWith :: FilePath -> Maybe Text -> String -> String -> (SolException -> Bool) -> TestTree
loadFailsWith fp c mode e p = testCase fp . catch tryLoad $ assertBool e . p where
  tryLoad = do
    let cfg = testConfig { solConf = testConfig.solConf { testMode = mode } }
    buildOutput <- compileContracts cfg.solConf (pure fp)
    void $ loadSolTests cfg buildOutput c
