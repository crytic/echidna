module Tests.StatefulSymbolic (statefulSymbolicTests) where

import Control.Concurrent (takeMVar)
import Control.Monad (foldM, forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Data.IORef (newIORef, readIORef)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import EVM.ABI (AbiValue(..))
import EVM.Dapp (DappInfo(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (Solver(Bitwuzla))
import EVM.Types (VM, VMType(Concrete))

import Common (loadSolTests, testConfig, solcV, withSolcVersion)
import Echidna.ABI (emptyDict)
import Echidna.Agent (runAgent, runAgentWithReady)
import Echidna.Exec (execTx)
import Echidna.Solidity (chooseContract, compileContracts)
import Echidna.SymExec.Common (extractErrors, extractTxs)
import Echidna.SymExec.Exploration (exploreContract)
import Echidna.Test (checkETest, createTest)
import Echidna.Types.Campaign (CampaignConf(..), initialWorkerState)
import Echidna.Types.Agent (Agent(..))
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestConf(..), TestType(..), TestValue(..), TestState(Solved), EchidnaTest(..), isOpen)
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), basicTx)
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..), WorkerType(..), WorkerStopReason(..))
import Echidna.Worker (pushCampaignEvent)

statefulSymbolicTests :: TestTree
statefulSymbolicTests = testGroup "Stateful symbolic execution" $
  [ solidityCase ("replay " ++ show name) $ do
      (vm, env, contract) <- load name False
      vm' <- prepare env vm
      (sequences, partials) <- solve env contract vm' "set"
      partials @?= []
      assertBool "counterexample found" (not $ null sequences)
      forM_ sequences $ \txs -> do
        map (.call) txs @?= [SolCall ("set", [AbiUInt 256 value])]
        replay env vm' txs (PropertyTest name env.cfg.solConf.contractAddr)
  | (name, value) <- [("echidna_value", 42), ("echidna_reverts", 43), ("echidna_revert_expected", 44)]
  ] ++
  [ solidityCase "block context and property sender survive the transaction boundary" $ do
      (vm, env, contract) <- load "echidna_context" False
      vm' <- prepare env vm
      (sequences, partials) <- solve env contract vm' "set"
      partials @?= []
      sequences @?= []
  , solidityCase "no-argument setter can falsify a property" $ do
      (vm, env, contract) <- load "echidna_noarg" False
      (sequences, partials) <- solve env contract vm "enable"
      partials @?= []
      assertBool "counterexample found" (not $ null sequences)
      forM_ sequences $ \txs -> replay env vm txs (PropertyTest "echidna_noarg" env.cfg.solConf.contractAddr)
  , solidityCase "assertion caller is independent of the preceding caller" $ do
      (vm, env, contract) <- load "check_sender" True
      vm' <- prepare env vm
      (sequences, partials) <- solve env contract vm' "set"
      partials @?= []
      assertBool "counterexample found" (not $ null sequences)
      forM_ sequences $ \txs -> do
        case txs of
          [setter, check] -> assertBool "different callers" (setter.src /= check.src)
          _ -> assertFailure "expected a setter and an assertion call"
        replay env vm' txs (AssertionTest False ("check_sender", []) env.cfg.solConf.contractAddr)
  , solidityCase "partial paths in the property are reported" $ do
      (vm, env, contract) <- load "echidna_loop" False
      vm' <- prepare env vm
      (sequences, partials) <- solve env contract vm' "set"
      sequences @?= []
      assertBool "incomplete property check is reported" (not $ null partials)
  , solidityCase "worker retains the corpus prefix in a replayable counterexample" $ do
      (vm, env, _) <- load "echidna_value" False
      stateRef <- newIORef initialWorkerState
      let prefix = [prepareTx env]
          cfg = env.cfg { campaignConf = env.cfg.campaignConf { workers = Just 0, symExecTargets = ["set"] } }
          worker = SymbolicAgent 0 vm emptyDict [("seed", prefix)] (Just "StatefulChecks") stateRef
      _ <- runAgent worker env { cfg }
      [test] <- traverse readIORef env.testRefs
      test.state @?= Solved
      map (.call) test.reproducer @?= [SolCall ("prepare", []), SolCall ("set", [AbiUInt 256 42])]
      replay env vm test.reproducer test.testType
  , solidityCase "worker respects symExecTargets for stateful checks" $ do
      (vm, env, _) <- load "echidna_value" False
      stateRef <- newIORef initialWorkerState
      let cfg = env.cfg { campaignConf = env.cfg.campaignConf { workers = Just 0, symExecTargets = ["enable"] } }
          worker = SymbolicAgent 0 vm emptyDict [("seed", [prepareTx env])] (Just "StatefulChecks") stateRef
      _ <- runAgent worker env { cfg }
      [test] <- traverse readIORef env.testRefs
      assertBool "unselected setter was not explored" (isOpen test)
  , solidityCase "coverage events retain the prefix when replacing the last call" $ do
      (vm, env, _) <- load "echidna_value" False
      stateRef <- newIORef initialWorkerState
      let txs = [prepareTx env, (prepareTx env) { call = SolCall ("set", [AbiUInt 256 0]) }]
          cfg = env.cfg { campaignConf = env.cfg.campaignConf { workers = Just 1, stopOnFail = True } }
          worker = SymbolicAgent 1 vm emptyDict [] (Just "StatefulChecks") stateRef
          publish event = pushCampaignEvent env (WorkerEvent 0 FuzzWorker event)
          ready = do
            publish NewCoverage { points = 1, numCodehashes = 1, corpusSize = 1, transactions = txs }
            publish (WorkerStopped TestLimitReached)
      _ <- runAgentWithReady ready worker env { cfg }
      [test] <- traverse readIORef env.testRefs
      map (.call) test.reproducer @?= [SolCall ("prepare", []), SolCall ("set", [AbiUInt 256 42])]
      replay env vm test.reproducer test.testType
  ]

solidityCase :: String -> IO () -> TestTree
solidityCase name = testCase name . withSolcVersion (Just (>= solcV (0,6,9)))

load :: Text -> Bool -> IO (VM Concrete, Env, SolcContract)
load name assertion = do
  let cfg = testConfig
        { solConf = testConfig.solConf { testMode = if assertion then "assertion" else "property", disableSlither = True }
        , testConf = testConfig.testConf { testSender = const 0xcafe }
        , campaignConf = testConfig.campaignConf { symExec = True, symExecSMTSolver = Bitwuzla, symExecMaxIters = 2 }
        }
  build <- compileContracts cfg.solConf ("symbolic/stateful.sol" :| [])
  (vm, env, _) <- loadSolTests cfg build (Just "StatefulChecks")
  contract <- chooseContract (Map.elems env.dapp.solcByName) (Just "StatefulChecks")
  test <- newIORef $ createTest $ if assertion
    then AssertionTest False (name, []) cfg.solConf.contractAddr
    else PropertyTest name cfg.solConf.contractAddr
  pure (vm, env { testRefs = [test] }, contract)

prepare :: Env -> VM Concrete -> IO (VM Concrete)
prepare env vm = snd <$> runReaderT (execTx vm (prepareTx env)) env

prepareTx :: Env -> Tx
prepareTx env = basicTx "prepare" [] (Set.elemAt 0 env.cfg.solConf.sender)
  env.cfg.solConf.contractAddr env.cfg.txConf.txGas (0, 0)

solve :: Env -> SolcContract -> VM Concrete -> Text -> IO ([[Tx]], [Text])
solve env contract vm name = do
  method <- maybe (assertFailure "method missing") pure $
    find (\m -> m.name == name) (Map.elems contract.abiMap)
  ((_, results), _) <- runReaderT
    (runStateT (exploreContract method vm) initialWorkerState) env
  (txs, partials) <- takeMVar results
  extractErrors txs @?= []
  pure (extractTxs txs, partials)

replay :: Env -> VM Concrete -> [Tx] -> TestType -> IO ()
replay env vm txs test = do
  endVm <- runReaderT (foldM (\v tx -> snd <$> execTx v tx) vm txs) env
  (value, _) <- runReaderT (checkETest (createTest test) endVm) env
  value @?= BoolValue False
