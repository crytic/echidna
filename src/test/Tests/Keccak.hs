{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Tests.Keccak (keccakTests) where

import Control.Concurrent (takeMVar)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Dapp (DappInfo(..))
import EVM.Solidity (Method(..), SolcContract(..))
import EVM.Solvers (Solver(..))
import EVM.Types (VM(..), VMType(Concrete), VMResult(..), keccak')

import Common (loadSolTests, testConfig)
import Echidna.Exec (execTx, pattern Reversion)
import Echidna.Solidity (chooseContract, compileContracts)
import Echidna.SymExec.Common (extractErrors, extractTxs)
import Echidna.SymExec.Exploration (exploreContract)
import Echidna.Types.Campaign (CampaignConf(..), initialWorkerState)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), basicTx)

keccakTests :: TestTree
keccakTests = testGroup "Keccak preimages" $
  concreteCase : map symbolicCase [Z3, Bitwuzla]
  where
    concreteCase = testCase "concrete-only campaigns do not collect preimages" $ do
      (vm, env, _) <- load False Z3
      assertBool "deployment did not collect preimages" $ Set.null vm.keccakPreImgs
      vm' <- remember env vm
      assertBool "transaction did not collect preimages" $ Set.null vm'.keccakPreImgs
      -- Hashing still works when recording is disabled.
      (result, _) <- runReaderT (execTx vm' (call env "checkHash" rememberedSecret)) env
      assertReversion result
    symbolicCase solver = testCase ("solver retains transaction preimages (" ++ show solver ++ ")") $ do
      (vm, env, contract) <- load True solver
      -- The first solver call needs the constructor's preimage.
      afterSymbolicTx <- solveAndReplay env contract vm constructorSecret
      -- Continue after replaying a transaction found by symbolic execution.
      afterRemember <- remember env afterSymbolicTx
      assertRecorded afterRemember constructorSecret
      _ <- solveAndReplay env contract afterRemember rememberedSecret
      pure ()

constructorSecret, rememberedSecret :: AbiValue
constructorSecret = AbiUInt 256 0x123456789abcdef0123456789abcdef
rememberedSecret = AbiUInt 256 0xfedcba98765432100123456789abcdef

load :: Bool -> Solver -> IO (VM Concrete, Env, SolcContract)
load symbolic solver = do
  let cfg = testConfig
        { solConf = testConfig.solConf { testMode = "assertion", disableSlither = True }
        , campaignConf = testConfig.campaignConf { symExec = symbolic, symExecSMTSolver = solver }
        }
  build <- compileContracts cfg.solConf ("symbolic/keccak-preimages.sol" :| [])
  (vm, env, _) <- loadSolTests cfg build (Just "KeccakPreimages")
  contract <- chooseContract (Map.elems env.dapp.solcByName) (Just "KeccakPreimages")
  pure (vm, env, contract)

call :: Env -> Text -> AbiValue -> Tx
call env name secret =
  basicTx name [secret] (Set.elemAt 0 env.cfg.solConf.sender) env.cfg.solConf.contractAddr
          env.cfg.txConf.txGas (0, 0)

remember :: Env -> VM Concrete -> IO (VM Concrete)
remember env vm = do
  (result, vm') <- runReaderT (execTx vm (call env "remember" rememberedSecret)) env
  case result of
    VMSuccess _ -> pure vm'
    _ -> assertFailure $ "remember transaction failed: " ++ show result

assertRecorded :: VM Concrete -> AbiValue -> IO ()
assertRecorded vm secret = do
  let preimage = encodeAbiValue secret
  assertBool "preimage survives across transactions" $
    Set.member (preimage, keccak' preimage) vm.keccakPreImgs

assertReversion :: VMResult Concrete -> IO ()
assertReversion result = case result of
  Reversion -> pure ()
  _ -> assertFailure $ "expected the hash assertion to fail, got " ++ show result

solveAndReplay :: Env -> SolcContract -> VM Concrete -> AbiValue -> IO (VM Concrete)
solveAndReplay env contract vm secret = do
  assertRecorded vm secret
  method <- maybe (assertFailure "checkHash method missing") pure $
    find (\m -> m.name == "checkHash") (Map.elems contract.abiMap)
  ((_, resultsChan), _) <- runReaderT
    (runStateT (exploreContract contract method vm) initialWorkerState) env
  (results, partials) <- takeMVar resultsChan
  extractErrors results @?= []
  partials @?= []
  let txs = extractTxs results
  -- Checking the actual argument ensures the solver received the preimage:
  -- an unconstrained Keccak model could otherwise invent a spurious match.
  map (.call) txs @?= [SolCall ("checkHash", [secret])]
  case txs of
    [tx] -> do
      (result, vm') <- runReaderT (execTx vm tx) env
      assertReversion result
      assertRecorded vm' secret
      pure vm'
    _ -> assertFailure "expected one counterexample"
