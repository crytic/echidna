module Tests.CalldataLimit (calldataLimitTests) where

import Control.Concurrent.STM (atomically, dupTChan, tryReadTChan)
import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value(..), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IORef (newIORef, readIORef)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, testCaseSteps, (@?=))

import EVM.ABI (AbiType(..))
import EVM.Solvers (Solver(Bitwuzla))

import Common (loadSolTests, solcV, testConfig, withSolcVersion)
import Echidna.ABI (emptyDict)
import Echidna.Agent (runAgent)
import Echidna.Config (adjustForVerificationMode)
import Echidna.Output.JSON qualified as JSON
import Echidna.Solidity (compileContracts)
import Echidna.SymExec.Calldata (minimumCalldataSize)
import Echidna.Types (ExecException(..))
import Echidna.Types.Agent (Agent(..))
import Echidna.Types.Campaign (CampaignConf(..), initialWorkerState)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), TestState(..), TestType(..), didFail, isSuccessful, isVerified)
import Echidna.Types.Tx (TxConf(..))
import Echidna.Types.Worker (CampaignEvent(..), WorkerEvent(..))
import Echidna.UI.Report (ppTS)

calldataLimitTests :: TestTree
calldataLimitTests = testGroup "Symbolic calldata limit"
  [ testCase "minimum ABI sizes include selectors, tuples and dynamic overhead" $ do
      let word = AbiUIntType 256
          receiver = AbiTupleType (V.fromList [AbiAddressType, word])
      minimumCalldataSize [] @?= 4
      minimumCalldataSize [AbiArrayType 60 receiver, word, word] @?= 3908
      minimumCalldataSize [AbiArrayType 63 receiver, word, word] @?= 4100
      minimumCalldataSize [AbiArrayType 100 receiver] @?= 6404
      minimumCalldataSize [AbiArrayType 200 receiver] @?= 12804
      minimumCalldataSize [AbiArrayType 127 word] @?= 4068
      minimumCalldataSize [AbiArrayType 127 word, AbiBytesDynamicType] @?= 4132
      minimumCalldataSize [AbiBytesDynamicType, AbiStringType, AbiArrayDynamicType receiver] @?= 196
      minimumCalldataSize [AbiArrayType 2 (AbiArrayType 3 word)] @?= 196
      minimumCalldataSize [AbiArrayType 2 AbiStringType] @?= 164
      minimumCalldataSize [AbiTupleType (V.fromList [word, AbiBytesDynamicType])] @?= 132
  , testCase "large nested dimensions cannot overflow the size calculation" $ do
      let count = maxBound :: Int
      minimumCalldataSize [AbiArrayType count (AbiArrayType count (AbiUIntType 256))]
        @?= 4 + 32 * toInteger count * toInteger count
  , testCaseSteps "oversized inputs fail verification before the solver starts" $ \step ->
      withSolcVersion (Just (>= solcV (0,6,9))) $ do
        let cfg = adjustForVerificationMode $ testConfig
              { solConf = testConfig.solConf
                  { testMode = "verification", disableSlither = True, sender = Set.singleton 0x10000 }
              , txConf = testConfig.txConf { maxValue = 0, maxTimeDelay = 0, maxBlockDelay = 0 }
              , campaignConf = testConfig.campaignConf
                  { symExecSMTSolver = Bitwuzla, symExecTimeout = 30, shrinkLimit = 0 }
              }
            assertion name tests = maybe (assertFailure $ "missing assertion " <> T.unpack name) pure $
              find (\test -> case test.testType of AssertionTest{sig = (n, _)} -> n == name; _ -> False) tests
        build <- compileContracts cfg.solConf ("symbolic/calldata-limit.sol" :| [])
        let run targets = do
              let configured = cfg { campaignConf = cfg.campaignConf { symExecTargets = targets } }
              (vm, env, _) <- loadSolTests configured build (Just "CalldataLimit")
              events <- atomically $ dupTChan env.eventQueue
              stateRef <- newIORef initialWorkerState
              _ <- runAgent (SymbolicAgent vm emptyDict (Just "CalldataLimit") stateRef) env
              tests <- traverse readIORef env.testRefs
              messages <- atomically $
                let drain = tryReadTChan events >>= maybe (pure []) (\event -> (event :) <$> drain)
                in drain
              pure (vm, env, tests, messages)
            sizeErrors messages = [err | (_, WorkerEvent _ _ (SymExecError err)) <- messages,
                                        "Cannot verify " `isPrefixOf` err]
        step "oversized false assertions become errors while a small method is verified"
        (vm, env, tests, messages) <- run ["checkTooLarge", "checkDynamicPrefix", "checkSmall"]
        forM_ [("checkTooLarge", 4100), ("checkDynamicPrefix", 4132)] $ \(name, bytes) -> do
          test <- assertion name tests
          message <- case test.state of
            Failed err@(SymbolicCalldataTooLarge signature required limit) -> do
              assertBool "diagnostic names the method" (name `T.isPrefixOf` signature)
              required @?= bytes
              limit @?= 4096
              pure $ show err
            state -> assertFailure $ "expected a calldata size error, got " <> show state
          isVerified test @?= False
          didFail test @?= False
          test.reproducer @?= []
          assertBool "error reaches the event stream" (message `elem` sizeErrors messages)
          rendered <- runReaderT (ppTS test.state test.result vm []) env
          assertBool "text reports the size error" (message `isInfixOf` rendered)
          case toJSON (JSON.mapTest env.dapp test) of
            Object fields -> do
              KeyMap.lookup "status" fields @?= Just (String "error")
              KeyMap.lookup "error" fields @?= Just (String $ T.pack message)
            _ -> assertFailure "expected a JSON test object"
          assertBool "oversized method never reaches solver setup" $ not $ or
            [T.unpack name `isInfixOf` msg | (_, WorkerEvent _ _ (SymExecLog msg)) <- messages]
        length (sizeErrors messages) @?= 2
        assertion "checkSmall" tests >>= \test -> isVerified test @?= True
        isSuccessful tests @?= False
        step "the below-limit false assertion is found"
        (_, _, below, belowMessages) <- run ["checkBelowLimit"]
        assertion "checkBelowLimit" below >>= \test -> didFail test @?= True
        sizeErrors belowMessages @?= []
        step "unselected oversized methods do not invalidate verification"
        (_, _, selected, selectedMessages) <- run ["checkSmall"]
        assertion "checkSmall" selected >>= \test -> isVerified test @?= True
        sizeErrors selectedMessages @?= []
        isSuccessful selected @?= True
  ]
