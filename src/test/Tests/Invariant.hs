{-# LANGUAGE GADTs #-}

-- Exercise the production adapter at multiple depths. Generic frontier caching
-- and driver semantics are tested upstream in hevm.
module Tests.Invariant (invariantTests) where

import Control.Monad (foldM, forM, forM_)
import Control.Monad.Reader (runReaderT)
import Data.List (sort)
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
import Echidna.Config ()
import Echidna.Exec (execTx)
import Echidna.Solidity (chooseContract, compileContracts)
import Echidna.SymExec.Common (extractErrors, extractTxs, symExecAbstractArith)
import Echidna.SymExec.Invariant (exploreMethod)
import Echidna.Test (checkETest, createTest)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestConf(..), TestType(..), TestValue(BoolValue))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))

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
        , campaignConf = testConfig.campaignConf { symExec = True, workers = Just 0, symExecSeqLen = depth }
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
  (results, partials) <- flip runReaderT runtime $
    withSolvers Bitwuzla 1 (Just 30) 1024 $ \solvers ->
      exploreMethod (head selected) properties vm 0x10000 cfg opts solvers Fetch.noRpc env.fetchSession
  extractErrors results @?= []
  partials @?= []
  pure (vm, env, properties, extractTxs results)
