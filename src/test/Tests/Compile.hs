module Tests.Compile (compilationTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Common (testConfig)
import Control.Lens (Prism', preview)
import Control.Monad (void)
import Control.Monad.Catch (catch)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Echidna.Types.Config
import Echidna.Types.Solidity (SolException, _ContractNotFound, _NoBytecode, _NoFuncs, _NoTests, _OnlyTests, _TestArgsFound, _ConstructorArgs, _DeploymentFailed)
import Echidna.Solidity (loadWithCryticCompile)

compilationTests :: TestTree
compilationTests = testGroup "Compilation and loading tests"
  [ loadFails "bad/nocontract.sol" (Just "c") "failed to warn on contract not found" $
      pmatch _ContractNotFound
  , loadFails "bad/nobytecode.sol" Nothing    "failed to warn on abstract contract" $
      pmatch _NoBytecode
  , loadFails "bad/nofuncs.sol"    Nothing    "failed to warn on no functions found" $
      pmatch _NoFuncs
  , loadFails "bad/notests.sol"    Nothing    "failed to warn on no tests found" $
      pmatch _NoTests
  , loadFails "bad/onlytests.sol"  Nothing    "failed to warn on no non-tests found" $
      pmatch _OnlyTests
  , loadFails "bad/testargs.sol"   Nothing    "failed to warn on test args found" $
      pmatch _TestArgsFound
  , loadFails "bad/consargs.sol"   Nothing    "failed to warn on cons args found" $
      pmatch _ConstructorArgs
  , loadFails "bad/revert.sol"     Nothing    "failed to warn on a failed deployment" $
      pmatch _DeploymentFailed
  , loadFails "basic/eip-170.sol"  Nothing    "failed to warn on a failed deployment" $
      pmatch _DeploymentFailed
  ]

loadFails :: FilePath -> Maybe Text -> String -> (SolException -> Bool) -> TestTree
loadFails fp c e p = testCase fp . catch tryLoad $ assertBool e . p where
  tryLoad = void $ loadWithCryticCompile testConfig._sConf (fp :| []) c

pmatch :: Prism' s a -> s -> Bool
pmatch p = isJust . preview p
