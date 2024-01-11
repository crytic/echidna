module Tests.Compile (compilationTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Common (testConfig)
import Control.Monad (void)
import Control.Monad.Catch (catch)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Echidna.Types.Solidity (SolException(..))
import Echidna.Solidity (loadSolTests)
import Echidna.Types.Config (Env(..))
import EVM.Dapp (emptyDapp)
import Data.IORef (newIORef)
import Control.Concurrent (newChan)

compilationTests :: TestTree
compilationTests = testGroup "Compilation and loading tests"
  [ loadFails "bad/nocontract.sol" (Just "c") "failed to warn on contract not found" $
      \case ContractNotFound _ -> True; _ -> False
  , loadFails "bad/nobytecode.sol" Nothing    "failed to warn on abstract contract" $
      \case NoBytecode _ -> True; _ -> False
  , loadFails "bad/nofuncs.sol"    Nothing    "failed to warn on no functions found" $
      \case NoFuncs -> True; _ -> False
  , loadFails "bad/notests.sol"    Nothing    "failed to warn on no tests found" $
      \case NoTests -> True; _ -> False
  , loadFails "bad/onlytests.sol"  Nothing    "failed to warn on no non-tests found" $
      \case OnlyTests -> True; _ -> False
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
  ]

loadFails :: FilePath -> Maybe Text -> String -> (SolException -> Bool) -> TestTree
loadFails fp c e p = testCase fp . catch tryLoad $ assertBool e . p where
  tryLoad = do
    codehashMap <- newIORef mempty
    cacheContracts <- newIORef mempty
    cacheSlots <- newIORef mempty
    eventQueue <- newChan
    coverageRef <- newIORef mempty
    corpusRef <- newIORef mempty
    testsRef <- newIORef mempty
    let env = Env { cfg = testConfig
                  , dapp = emptyDapp
                  , codehashMap
                  , fetchContractCache = cacheContracts
                  , fetchSlotCache = cacheSlots
                  , chainId = Nothing
                  , eventQueue
                  , coverageRef
                  , corpusRef
                  , testsRef
                  }
    void $ loadSolTests env (fp :| []) c
