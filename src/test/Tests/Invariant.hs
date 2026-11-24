{-# LANGUAGE GADTs #-}

-- Exercise the production adapter at multiple depths. Generic frontier caching
-- and driver semantics are tested upstream in hevm.
module Tests.Invariant (invariantTests) where

import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (foldM, forM, forM_)
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef, readIORef)
import Data.List (find, isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Yaml qualified as Y
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import EVM.ABI (AbiValue(..))
import EVM.Dapp (DappInfo(..))
import EVM.Effects qualified as HEVM
import EVM.Fetch qualified as Fetch
import EVM.Solidity (Method(..), SolcContract(..))
import EVM.Solvers (Solver(Bitwuzla), withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic(StackBased), VeriOpts(..))
import EVM.Types (VM, VMType(..))

import Common (loadSolTests, solcV, testConfig, withSolcVersion)
import Echidna.ABI (emptyDict)
import Echidna.Agent (runAgent)
import Echidna.Config (adjustForVerificationMode)
import Echidna.Exec (execTx)
import Echidna.Solidity (chooseContract, compileContracts)
import Echidna.SymExec.Common (extractErrors, extractTxs, symExecAbstractArith)
import Echidna.SymExec.Invariant (exploreSearch, invariantSearch, searchMethods)
import Echidna.Test (checkETest, createTest)
import Echidna.Types.Agent (Agent(..))
import Echidna.Types.Campaign (CampaignConf(..), initialWorkerState)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (Filter(..), SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), TestConf(..), TestType(..), TestValue(BoolValue), didFail, isOpen, isPassed, isVerified)
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..))

invariantTests :: TestTree
invariantTests = testGroup "Native invariant integration"
  [ solidityCase "one call cannot falsify the properties" $ do
      (_, _, _, txs) <- experiment 1
      txs @?= []
  , solidityCase "configured two-call search yields replayable native counterexamples" $ do
      (vm, env, properties, sequences) <- experiment 2
      length sequences @?= 2
      forM_ sequences $ \txs -> do
        map (.call) txs @?= [SolCall ("advance", [AbiUInt 256 1]), SolCall ("advance", [AbiUInt 256 2])]
        endVm <- runReaderT (foldM (\v tx -> snd <$> execTx v tx) vm txs) env
        checked <- forM properties $ \test -> do
          (value, _) <- runReaderT (checkETest (createTest test) endVm) env
          pure (test, value)
        sort [name | (PropertyTest name _, BoolValue False) <- checked]
          @?= ["echidna_not_two", "echidna_revert_expected"]
  , testCase "symbolic sequence length must be positive" $ do
      forM_ ["symExecSeqLen: 0", "symExecSeqLen: -1"] $ \yaml ->
        case Y.decodeEither' yaml of
          Right (_ :: EConfig) -> assertFailure "invalid sequence length accepted"
          Left _ -> pure ()
      case Y.decodeEither' "symExecSeqLen: 3" of
        Right (cfg :: EConfig) -> cfg.campaignConf.symExecSeqLen @?= 3
        Left err -> assertFailure $ show err
  , solidityCase "mixed methods respect depth, target selection and replay" $ do
      (vm, env, contract) <- mixedFixture
      let run depth targets = solveMixed vm contract env
            { cfg = env.cfg { campaignConf = env.cfg.campaignConf
                { symExecSeqLen = depth, symExecTargets = targets } } }
      run 1 [] >>= (@?= [])
      sequences <- run 2 []
      length sequences @?= 1
      forM_ sequences $ \txs -> checkMixedReplay vm env txs
      run 2 ["arm"] >>= (@?= [])
      run 2 ["trigger"] >>= (@?= [])
      exact <- run 2 ["arm(uint256)", "trigger(uint256)"]
      length exact @?= 1
      forM_ exact $ \txs -> checkMixedReplay vm env txs
      let blocked = env { cfg = env.cfg { solConf = env.cfg.solConf
            { methodFilter = Blacklist ["MixedInvariant.trigger(uint256)"] } } }
      solveMixed vm contract blocked >>= (@?= [])
  , solidityCase "transition selection excludes checks, view/pure methods and opt-outs" $ do
      (_, env, contract) <- mixedFixture
      tests <- traverse readIORef env.testRefs
      let selected cfg = sort $ maybe [] (map (.methodSignature) . searchMethods) (invariantSearch cfg contract tests)
          targets names = env.cfg { campaignConf = env.cfg.campaignConf { symExecTargets = names } }
      selected env.cfg @?= ["arm(bytes32)", "arm(uint256)", "trigger(uint256)"]
      selected (targets ["arm(uint256)", "trigger", "echidna_mutator", "shortcut_no_symexec", "readView", "readPure"])
        @?= ["arm(uint256)", "trigger(uint256)"]
      selected (targets ["shortcut_no_symexec"]) @?= []
      selected (env.cfg { solConf = env.cfg.solConf
        { methodFilter = Whitelist ["MixedInvariant.arm(uint256)", "MixedInvariant.trigger(uint256)"] } })
        @?= ["arm(uint256)", "trigger(uint256)"]
  , solidityCase "symbolic-only worker runs one shared mixed-method search" $ do
      (vm, env, _) <- mixedFixture
      stateRef <- newIORef initialWorkerState
      events <- atomically $ dupTChan env.eventQueue
      _ <- runAgent (SymbolicAgent 0 vm emptyDict [] (Just "MixedInvariant") stateRef) env
      tests <- traverse readIORef env.testRefs
      failed <- maybe (assertFailure "missing property") pure $
        find (\t -> t.testType == PropertyTest "echidna_never_finished" env.cfg.solConf.contractAddr) tests
      isOpen failed @?= False
      checkMixedReplay vm env failed.reproducer
      messages <- atomically $
        let drain = tryReadTChan events >>= maybe (pure []) (\event -> (event :) <$> drain)
        in drain
      length [() | (_, WorkerEvent _ _ (SymExecLog message)) <- messages,
                   "Checking invariants over " `isPrefixOf` message] @?= 1
      [err | (_, WorkerEvent _ _ (SymExecError err)) <- messages] @?= []
  , solidityCase "verification bounds include the failing call and preserve bounded result status" boundedVerification
  ]
  where
    solidityCase name = testCase name . withSolcVersion (Just (>= solcV (0,6,9)))

experiment :: Int -> IO (VM Concrete, Env, [TestType], [[Tx]])
experiment depth = do
  let cfg = testConfig
        { solConf = testConfig.solConf
            { testMode = "property", disableSlither = True, sender = Set.singleton 0x10000 }
        , testConf = testConfig.testConf { testSender = const 0xcafe }
        , txConf = testConfig.txConf { maxTimeDelay = 0, maxBlockDelay = 0 }
        , campaignConf = testConfig.campaignConf { symExec = True, workers = Just 0, symExecSeqLen = depth, symExecTargets = ["advance"] }
        }
  build <- compileContracts cfg.solConf ("symbolic/native-invariant.sol" :| [])
  (vm, env, _) <- loadSolTests cfg build (Just "NativeInvariant")
  contract <- chooseContract (Map.elems env.dapp.solcByName) (Just "NativeInvariant")
  let selected = [m | m <- Map.elems contract.abiMap, m.methodSignature == "advance(uint256)"]
      properties = [PropertyTest m.name cfg.solConf.contractAddr
                   | m <- Map.elems contract.abiMap, cfg.solConf.prefix `T.isPrefixOf` m.name]
      runtime = HEVM.defaultEnv { HEVM.config = HEVM.defaultConfig { HEVM.abstractArith = symExecAbstractArith } }
      opts = VeriOpts (IterConfig (Just 3) (Just 3) 1 StackBased) Fetch.noRpc
  length selected @?= 1
  length properties @?= 4
  search <- maybe (assertFailure "missing invariant search") pure $ invariantSearch cfg contract (map createTest properties)
  (results, partials) <- flip runReaderT runtime $
    withSolvers Bitwuzla 1 (Just 30) 1024 $ \solvers ->
      exploreSearch search vm 0x10000 cfg opts solvers Fetch.noRpc env.fetchSession
  extractErrors results @?= []
  partials @?= []
  pure (vm, env, properties, extractTxs results)

mixedFixture :: IO (VM Concrete, Env, SolcContract)
mixedFixture = do
  let cfg = testConfig
        { solConf = testConfig.solConf
            { testMode = "property", disableSlither = True, sender = Set.singleton 0x10000 }
        , txConf = testConfig.txConf { maxTimeDelay = 0, maxBlockDelay = 0 }
        , campaignConf = testConfig.campaignConf
            { symExec = True, symExecSMTSolver = Bitwuzla, workers = Just 0, symExecSeqLen = 2, shrinkLimit = 0 }
        }
  build <- compileContracts cfg.solConf ("symbolic/mixed-invariant.sol" :| [])
  (vm, env, _) <- loadSolTests cfg build (Just "MixedInvariant")
  contract <- chooseContract (Map.elems env.dapp.solcByName) (Just "MixedInvariant")
  pure (vm, env, contract)

solveMixed :: VM Concrete -> SolcContract -> Env -> IO [[Tx]]
solveMixed vm contract env = do
  tests <- traverse readIORef env.testRefs
  search <- maybe (assertFailure "missing invariant search") pure $ invariantSearch env.cfg contract tests
  let opts = VeriOpts (IterConfig (Just 3) (Just 3) 1 StackBased) Fetch.noRpc
      runtime = HEVM.defaultEnv { HEVM.config = HEVM.defaultConfig { HEVM.abstractArith = symExecAbstractArith } }
  (results, partials) <- flip runReaderT runtime $
    withSolvers Bitwuzla 1 (Just 30) 1024 $ \solvers ->
      exploreSearch search vm 0x10000 env.cfg opts solvers Fetch.noRpc env.fetchSession
  extractErrors results @?= []
  partials @?= []
  pure (extractTxs results)

checkMixedReplay :: VM Concrete -> Env -> [Tx] -> IO ()
checkMixedReplay vm env txs = do
  map (.call) txs @?= [SolCall ("arm", [AbiUInt 256 41]), SolCall ("trigger", [AbiUInt 256 42])]
  end <- runReaderT (foldM (\v tx -> snd <$> execTx v tx) vm txs) env
  let test = createTest $ PropertyTest "echidna_never_finished" env.cfg.solConf.contractAddr
  (value, _) <- runReaderT (checkETest test end) env
  value @?= BoolValue False

boundedVerification :: IO ()
boundedVerification = do
  let cfg = testConfig
        { solConf = testConfig.solConf
            { testMode = "verification", disableSlither = True, sender = Set.singleton 0x10000 }
        , txConf = testConfig.txConf { maxTimeDelay = 0, maxBlockDelay = 0 }
        , campaignConf = testConfig.campaignConf { symExecSMTSolver = Bitwuzla, shrinkLimit = 0 }
        }
  build <- compileContracts cfg.solConf ("symbolic/bounded-verification.sol" :| [])
  forM_ [(1, []), (2, []), (3, []), (3, ["trigger", "checkFinished"])] $ \(depth, targets) -> do
    let config = adjustForVerificationMode $ cfg
          { campaignConf = cfg.campaignConf { symExecSeqLen = depth, symExecTargets = targets } }
    (vm, env, _) <- loadSolTests config build (Just "BoundedVerification")
    stateRef <- newIORef initialWorkerState
    events <- atomically $ dupTChan env.eventQueue
    _ <- runAgent (SymbolicAgent 0 vm emptyDict [] (Just "BoundedVerification") stateRef) env
    tests <- traverse readIORef env.testRefs
    let assertion name = maybe (assertFailure $ "missing assertion " <> T.unpack name) pure $
          find (\t -> case t.testType of AssertionTest{sig = (n, _)} -> n == name; _ -> False) tests
    trigger <- assertion "trigger"
    finished <- assertion "checkFinished"
    arm <- assertion "arm"
    shortcut <- assertion "shortcut_no_symexec"
    didFail trigger @?= (depth >= 2 && null targets)
    didFail finished @?= (depth >= 3 && null targets)
    isOpen shortcut @?= True
    if depth == 1 then do
      isVerified trigger @?= True
      isVerified finished @?= True
    else do
      any isVerified tests @?= False
      isPassed arm @?= null targets
      if null targets then do
        map (.call) trigger.reproducer @?=
          [SolCall ("arm", [AbiUInt 256 41]), SolCall ("trigger", [AbiUInt 256 42])]
        if depth == 3 then
          map (.call) finished.reproducer @?=
            [SolCall ("arm", [AbiUInt 256 41]), SolCall ("prepare", []), SolCall ("checkFinished", [AbiUInt 256 43])]
        else isPassed finished @?= True
      else do
        isOpen arm @?= True
        isPassed trigger @?= True
        isPassed finished @?= True
    messages <- atomically $
      let drain = tryReadTChan events >>= maybe (pure []) (\event -> (event :) <$> drain)
      in drain
    [err | (_, WorkerEvent _ _ (SymExecError err)) <- messages] @?= []
    length [() | (_, WorkerEvent _ _ (SymExecLog message)) <- messages,
                 "Checking bounded assertions over " `isPrefixOf` message] @?= (if depth > 1 then 1 else 0)
