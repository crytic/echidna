module Tests.SymbolicContext (symbolicContextTests, symbolicNoCallTests) where

import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set qualified as Set
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
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), TestState(Unsolvable), didFail)
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..))

symbolicContextTests :: TestTree
symbolicContextTests = testCaseSteps "symbolic ETH, timestamp and block number replay" $ \step ->
  withSolcVersion (Just (>= solcV (0,6,9))) $ do
    let cfg = testConfig
          { solConf = testConfig.solConf
              { testMode = "property", disableSlither = True, sender = Set.singleton 0x10000, balanceAddr = 0 }
          , txConf = testConfig.txConf { maxValue = 6, maxTimeDelay = 2, maxBlockDelay = 1 }
          , campaignConf = testConfig.campaignConf
              { symExec = True, symExecSMTSolver = Bitwuzla, workers = Just 0, shrinkLimit = 0
              , symExecSeqLen = 2, symExecTargets = ["deposit", "finish"] }
          }
        verification depth selectedTargets = adjustForVerificationMode $ cfg
          { solConf = cfg.solConf { testMode = "verification" }
          , campaignConf = cfg.campaignConf { symExecSeqLen = depth, symExecTargets = selectedTargets }
          }
        paidCalls = [SolCall ("deposit", []), SolCall ("finish", [])]
        targets = ["deposit", "finish", "checkFinished"]
    build <- compileContracts cfg.solConf ("symbolic/paid-context.sol" :| [])
    forM_
      [ ("property after two paid calls", cfg, paidCalls, [3, 6])
      , ("value bound excludes the second payment", cfg { txConf = cfg.txConf { maxValue = 5 } }, [], [])
      , ("time bound excludes the first payment", cfg { txConf = cfg.txConf { maxTimeDelay = 1 } }, [], [])
      , ("block bound excludes the first payment", cfg { txConf = cfg.txConf { maxBlockDelay = 0 } }, [], [])
      , ("two calls cannot reach the assertion", verification 2 targets, [], [])
      , ("bounded assertion with paid setup", verification 3 targets,
          paidCalls <> [SolCall ("checkFinished", [AbiUInt 256 44])], [3, 6, 0])
      , ("single-call verifier solves context without ABI arguments", verification 1 [],
          [SolCall ("single", [])], [5])
      ] $ \(label, config, expectedCalls, expectedValues) -> do
        step label
        (vm, env, _) <- loadSolTests config build (Just "PaidContext")
        stateRef <- newIORef initialWorkerState
        events <- atomically $ dupTChan env.eventQueue
        _ <- runAgent (SymbolicAgent 0 vm emptyDict [] (Just "PaidContext") stateRef) env
        tests <- traverse readIORef env.testRefs
        case filter didFail tests of
          [] -> expectedCalls @?= []
          [failed] -> do
            assertBool "unexpected failure" (not $ null expectedCalls)
            map (.call) failed.reproducer @?= expectedCalls
            map (.value) failed.reproducer @?= expectedValues
            let paid = filter ((> 0) . (.value)) failed.reproducer
            map (.delay) paid @?= replicate (length paid) (2, 1)
            forM_ failed.reproducer $ \tx -> do
              tx.src @?= 0x10000
              assertBool "bounded time and block delays" $
                fst tx.delay <= config.txConf.maxTimeDelay && snd tx.delay <= config.txConf.maxBlockDelay
          _ -> assertFailure "unexpected additional failures"
        forM_ tests $ \test -> case test.state of
          Unsolvable bound -> do
            config.solConf.testMode @?= "verification"
            bound @?= config.campaignConf.symExecSeqLen
          _ -> pure ()
        messages <- atomically $
          let drain = tryReadTChan events >>= maybe (pure []) (\event -> (event :) <$> drain)
          in drain
        [err | (_, WorkerEvent _ _ (SymExecError err)) <- messages] @?= []

symbolicNoCallTests :: TestTree
symbolicNoCallTests = testCaseSteps "symbolic NoCall steps replay through the campaign" $ \step ->
  withSolcVersion (Just (>= solcV (0,6,9))) $ do
    let cfg = testConfig
          { solConf = testConfig.solConf
              { testMode = "property", disableSlither = True, sender = Set.singleton 0x10000 }
          , txConf = testConfig.txConf { maxValue = 0, maxTimeDelay = 1, maxBlockDelay = 1 }
          , campaignConf = testConfig.campaignConf
              { symExec = True, symExecSMTSolver = Bitwuzla, workers = Just 0, shrinkLimit = 0 }
          }
        config mode depth target = adjustForVerificationMode $ cfg
          { solConf = cfg.solConf { testMode = mode }
          , campaignConf = cfg.campaignConf { symExecSeqLen = depth, symExecTargets = [target] }
          }
        waited name = [NoCall, SolCall (name, [])]
    build <- compileContracts cfg.solConf ("symbolic/nocall.sol" :| [])
    forM_
      [ ("one step cannot capture the deadline", config "property" 1 "capture", [])
      , ("wait then capture falsifies native properties", config "property" 2 "capture", waited "capture")
      , ("native properties are not checked directly after a wait", config "property" 2 "blocked", [])
      , ("one transaction cannot fail the assertion", config "verification" 1 "assertAfterWait", [])
      , ("wait then no-argument assertion", config "verification" 2 "assertAfterWait", waited "assertAfterWait")
      , ("assertion mode checks after two context-only steps", config "assertion" 2 "assertAfterWait",
          [NoCall, NoCall, SolCall ("assertAfterWait", [])])
      , ("time bound excludes the failure", (config "verification" 2 "assertAfterWait")
          { txConf = cfg.txConf { maxTimeDelay = 0 } }, [])
      , ("block bound excludes the failure", (config "verification" 2 "assertAfterWait")
          { txConf = cfg.txConf { maxBlockDelay = 0 } }, [])
      ] $ \(label, configured, expected) -> do
        step label
        (vm, env, _) <- loadSolTests configured build (Just "NoCallContext")
        stateRef <- newIORef initialWorkerState
        events <- atomically $ dupTChan env.eventQueue
        _ <- runAgent (SymbolicAgent 0 vm emptyDict [] (Just "NoCallContext") stateRef) env
        tests <- traverse readIORef env.testRefs
        let failed = filter didFail tests
        assertBool "expected failures were replayed" (null failed == null expected)
        forM_ failed $ \test -> do
          map (.call) test.reproducer @?= expected
          forM_ test.reproducer $ \tx -> do
            tx.value @?= 0
            assertBool "bounded wait and call delays" (fst tx.delay <= 1 && snd tx.delay <= 1)
        messages <- atomically $
          let drain = tryReadTChan events >>= maybe (pure []) (\event -> (event :) <$> drain)
          in drain
        [err | (_, WorkerEvent _ _ (SymExecError err)) <- messages] @?= []
