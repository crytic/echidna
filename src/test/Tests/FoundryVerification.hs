module Tests.FoundryVerification (foundryVerificationTests) where

import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef)
import Data.List (find, isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCaseSteps, (@?=))

import EVM.ABI (AbiValue(..))
import EVM.Solvers (Solver(Bitwuzla))

import Common (loadSolTests, solcV, testConfig, withSolcVersion)
import Echidna.ABI (emptyDict)
import Echidna.Agent (runAgent)
import Echidna.Config (adjustForVerificationMode)
import Echidna.Solidity (compileContracts)
import Echidna.Types.Agent (Agent(..))
import Echidna.Types.Campaign (CampaignConf(..), initialWorkerState)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (Filter(..), SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), TestState(Unsolvable), TestType(..), didFail, isOpen, isVerified)
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..))

foundryVerificationTests :: TestTree
foundryVerificationTests = testCaseSteps "Foundry harnesses retain their semantics in verification mode" $ \step ->
  withSolcVersion (Just (>= solcV (0,6,9))) $ do
    let cfg = testConfig
          { solConf = testConfig.solConf
              { testMode = "verification", disableSlither = True, sender = Set.singleton 0x10000 }
          , txConf = testConfig.txConf { maxValue = 7, maxTimeDelay = 0, maxBlockDelay = 0 }
          , campaignConf = testConfig.campaignConf
              { symExecSMTSolver = Bitwuzla, shrinkLimit = 0, symExecTimeout = 30 }
          }
        config depth targets = adjustForVerificationMode $ cfg
          { campaignConf = cfg.campaignConf { symExecSeqLen = depth, symExecTargets = targets } }
        assertion name tests = maybe (assertFailure $ "missing test " <> T.unpack name) pure $
          find (\t -> case t.testType of AssertionTest{sig = (n, _)} -> n == name; _ -> False) tests
    build <- compileContracts cfg.solConf ("symbolic/foundry-verification.sol" :| [])
    let run contract configured partial = do
          (vm, env, _) <- loadSolTests configured build (Just contract)
          events <- atomically $ dupTChan env.eventQueue
          stateRef <- newIORef initialWorkerState
          _ <- runAgent (SymbolicAgent 0 vm emptyDict [] (Just contract) stateRef) env
          tests <- traverse readIORef env.testRefs
          forM_ tests $ \test -> case test.testType of
            AssertionTest{foundry, sig = (name, _)} -> do
              foundry @?= (contract /= "NativeRevertVerification")
              assertBool "setup and marker are not tests" (name `notElem` ["setUp", "IS_TEST"])
            _ -> assertFailure "unexpected test type"
          forM_ (filter didFail tests) $ \test -> do
            assertBool "counterexample fits the transaction bound" $
              length test.reproducer <= configured.campaignConf.symExecSeqLen
            forM_ test.reproducer $ \tx -> tx.value @?= 0
          messages <- atomically $
            let drain = tryReadTChan events >>= maybe (pure []) (\event -> (event :) <$> drain)
            in drain
          let errors = [err | (_, WorkerEvent _ _ (SymExecError err)) <- messages]
          if partial then do
            assertBool "incomplete search is reported" (any ("Partial path" `isPrefixOf`) errors)
            any isVerified tests @?= False
          else errors @?= []
          pure tests
    forM_ [1, 2, 3] $ \depth -> do
      step $ "mixed actions followed by an invariant, bound " <> show depth
      tests <- run "FoundryBoundedVerification" (config depth []) False
      check <- assertion "invariant_notFinished" tests
      if depth == 3 then do
        didFail check @?= True
        map (.call) check.reproducer @?=
          [SolCall ("arm", [AbiUInt 256 41]), SolCall ("finish", []), SolCall ("invariant_notFinished", [])]
      else check.state @?= Unsolvable depth
      forM_ tests $ \test -> case test.testType of
        AssertionTest{sig = ("shortcut_no_symexec", _)} -> isOpen test @?= True
        _ -> pure ()
    step "target and function filters apply to every step"
    forM_
      [ config 3 ["arm(uint256)", "invariant_notFinished()"]
      , (config 3 []) { solConf = cfg.solConf
          { methodFilter = Blacklist ["FoundryBoundedVerification.finish()"] } }
      ] $ \configured -> do
        tests <- run "FoundryBoundedVerification" configured False
        check <- assertion "invariant_notFinished" tests
        check.state @?= Unsolvable 3
    forM_ [2, 3] $ \depth -> do
      step $ "mutating invariant retains writes, bound " <> show depth
      tests <- run "FoundryMutatingVerification" (config depth []) False
      check <- assertion "statefulFuzz_observations" tests
      if depth == 3 then do
        didFail check @?= True
        map (.call) check.reproducer @?= replicate 3 (SolCall ("statefulFuzz_observations", []))
      else check.state @?= Unsolvable depth
    step "ordinary reverts, expected failures, assumptions and zero-value calls"
    tests <- run "FoundryFailureVerification" (config 1 []) False
    forM_ ["test_revert", "check_revert", "prove_revert", "testFail_sometimesReverts"] $ \name ->
      assertion name tests >>= \test -> didFail test @?= True
    forM_ ["testFail_revert", "test_assume", "testFail_assume", "invariant_context"] $ \name ->
      assertion name tests >>= \test -> test.state @?= Unsolvable 1
    failing <- assertion "testFail_sometimesReverts" tests
    map (.call) failing.reproducer @?= [SolCall ("testFail_sometimesReverts", [AbiUInt 256 42])]
    step "ordinary handler reverts retain Foundry fuzzing semantics"
    handlers <- run "FoundryFailureVerification" (config 3 ["handlerRevert"]) False
    assertion "handlerRevert" handlers >>= \test -> test.state @?= Unsolvable 3
    step "native contracts still ignore ordinary reverts"
    native <- run "NativeRevertVerification" (config 1 []) False
    assertion "test_revert" native >>= \test -> test.state @?= Unsolvable 1
    step "partial paths prevent a verified result"
    let bounded = config 3 ["arm", "invariant_notFinished"]
    _ <- run "FoundryBoundedVerification"
      bounded { campaignConf = bounded.campaignConf { symExecMaxExplore = 0 } } True
    pure ()
