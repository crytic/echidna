module Tests.ConstraintDiscovery (constraintDiscoveryTests) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Random.Strict (evalRand, mkStdGen)
import Control.Monad.State.Strict (evalStateT)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import EVM.ABI (AbiValue(..))
import EVM.Types (Expr(..), Prop(..))

import Common (testConfig, withSolcVersion, solcV)
import Echidna (prepareContract)
import Echidna.Solidity (compileContracts)
import Echidna.SymExec.Bounds (inferArgumentBounds)
import Echidna.Types.Campaign
  ( CampaignConf(..), WorkerState(..), initialWorkerState )
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Constraint (ConstraintKey(..), NumericBounds(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), basicTx)
import Echidna.Types.Worker (WorkerType(..))
import Echidna.UI (ui)
import Echidna.Worker (getNWorkers, workerIDToType)
import Echidna.Worker.Sequence (applyCachedBounds)

constraintDiscoveryTests :: TestTree
constraintDiscoveryTests = testGroup "Constraint discovery"
  [ testCase "intersects constraints within a successful path" $ do
      let bounds = inferArgumentBounds
            [ [ PGEq (Var "arg1") (Lit 10)
              , PLT (Var "arg1") (Lit 20)
              , PLEq (Add (Var "arg2") (Lit 1)) (Lit 100)
              ]
            ]
      Map.lookup 0 bounds @?= Just (NumericBounds (Just 10) (Just 19))
      Map.lookup 1 bounds @?= Just (NumericBounds Nothing (Just 99))

  , testCase "widens ordinary branches across successful paths" $ do
      let bounds = inferArgumentBounds
            [ [PGEq (Var "arg1") (Lit 10), PLT (Var "arg1") (Lit 20)]
            , [PGEq (Var "arg1") (Lit 20)]
            ]
      Map.lookup 0 bounds @?= Just (NumericBounds (Just 10) Nothing)

  , testCase "drops constraints absent from another successful path" $ do
      let bounds = inferArgumentBounds
            [ [PLEq (Var "arg1") (Lit 20)]
            , [PLEq (Var "arg2") (Lit 30)]
            ]
      bounds @?= Map.empty

  , testCase "adds exactly one helper alongside symbolic execution" $ do
      let conf = testConfig.campaignConf
            { workers = Just 2
            , symExec = True
            , constraintDiscovery = True
            }
      getNWorkers conf @?= 4
      (workerIDToType conf <$> [0..3]) @?=
        [FuzzWorker, FuzzWorker, ConstraintDiscoveryWorker, SymbolicWorker]

  , testCase "applies bounds only to the exact structural prefix" $ do
      let sender = Set.elemAt 0 testConfig.solConf.sender
          tx name arguments = basicTx name arguments sender testConfig.solConf.contractAddr
            testConfig.txConf.txGas (0, 0)
          lower = tx "lowerLimit" []
          differentPrefix = tx "otherState" []
          constrained = tx "constrained"
            [AbiUInt 256 31, AbiUInt 256 0, AbiUInt 256 0x99, AbiUInt 256 0]
          key = ConstraintKey
            { prefix = [lower]
            , signature = "constrained(uint256,uint256,uint256,uint256)"
            , sender = constrained.src
            , destination = constrained.dst
            , callValue = constrained.value
            , callGas = constrained.gas
            , callGasPrice = constrained.gasprice
            , callDelay = constrained.delay
            }
          state = initialWorkerState
            { constraintBounds = Map.singleton key $
                Map.singleton 3 (NumericBounds (Just 0x123) (Just 0x123))
            }
          run prefix seed = evalRand
            (evalStateT (applyCachedBounds [prefix, constrained]) state)
            (mkStdGen seed)
          fourthArgument [_, Tx{call = SolCall (_, [_, _, _, AbiUInt _ value])}] = Just value
          fourthArgument _ = Nothing
          matching = fourthArgument . run lower <$> [0..30]
          different = fourthArgument . run differentPrefix <$> [0..30]
      assertBool "an exact prefix should use the discovered bound" $
        Just 0x123 `elem` matching
      assertBool "a different prefix must not use the cached bound" $
        all (== Just 0) different

  , testCase "discovery worker updates the fuzz worker" $
      withSolcVersion (Just (>= solcV (0,8,13))) $ do
        let source = "symbolic/constraint-discovery.sol"
            selectedContract = Just "ConstraintDiscoveryTest"
            cfg = testConfig
              { campaignConf = testConfig.campaignConf
                  { constraintDiscovery = True
                  , workers = Just 1
                  , testLimit = 10000
                  , seqLen = 1
                  }
              }
        build <- compileContracts cfg.solConf (source :| [])
        (vm, env, dict) <- prepareContract cfg (source :| []) build selectedContract 0
        let sender = Set.elemAt 0 cfg.solConf.sender
            tx name arguments = basicTx name arguments sender cfg.solConf.contractAddr
              cfg.txConf.txGas (0, 0)
            lower = tx "lowerLimit" []
            constrained = tx "constrained"
              [AbiUInt 256 31, AbiUInt 256 0, AbiUInt 256 0x99, AbiUInt 256 0]
        states <- runReaderT
          (ui vm dict [("stateful", [lower, constrained])] selectedContract) env
        fuzzState <- maybe (assertFailure "fuzz worker state missing") pure $
          find ((== 0) . (.workerId)) states
        let initialBounds = snd <$> find
              (\(key, _) -> null key.prefix
                && key.signature == "constrained(uint256,uint256,uint256,uint256)")
              (Map.toList fuzzState.constraintBounds)
            statefulBounds = snd <$> find
              (\(key, _) -> key.prefix == [lower])
              (Map.toList fuzzState.constraintBounds)
        (initialBounds >>= Map.lookup 0)
          @?= Just (NumericBounds (Just 31) (Just $ 2 ^ (256 :: Int) - 2))
        (initialBounds >>= Map.lookup 2)
          @?= Just (NumericBounds (Just 153) (Just $ 2 ^ (256 :: Int) - 1))
        (initialBounds >>= Map.lookup 3)
          @?= Just (NumericBounds (Just 0) (Just 0x888))
        (statefulBounds >>= Map.lookup 3)
          @?= Just (NumericBounds (Just 0) (Just 0x123))
  ]
