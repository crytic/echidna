module Tests.Replay (replayTests) where

import Control.Monad.Reader (runReaderT)
import Data.Aeson (FromJSON(..), eitherDecodeStrict, withObject, (.:), (.:?))
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import EVM.ABI (AbiValue(..))
import EVM.Types (VM, VMType(Concrete))

import Echidna.Solidity (compileContracts)
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Corpus (corpusSize)
import Echidna.Types.Coverage (coverageStats)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx, TxConf(..), basicTx)
import Echidna.Worker.Replay (executeSeq)

import Common (loadSolTests, solcV, testConfig, withSolcVersion)

-- | The parts of the report the assertions below look at.
data Report = Report
  { status :: Text
  , transactionCount :: Int
  , failedTxIndex :: Maybe Int
  , transactions :: [TxReport]
  , trace :: Maybe Text
  }

instance FromJSON Report where
  parseJSON = withObject "report" $ \o -> Report
    <$> o .: "status"
    <*> o .: "transaction_count"
    <*> o .: "failed_tx_index"
    <*> o .: "transactions"
    <*> o .:? "trace"

data TxReport = TxReport
  { index :: Int
  , call :: Text
  , status :: Text
  , result :: Text
  , gasUsed :: Word64
  , logs :: [Text]
  }

instance FromJSON TxReport where
  parseJSON = withObject "transaction" $ \o -> TxReport
    <$> o .: "index"
    <*> o .: "call"
    <*> o .: "status"
    <*> o .: "result"
    <*> o .: "gas_used"
    <*> o .: "logs"

replayTests :: TestTree
replayTests = testGroup "Sequence replay"
  [ testCase "reports every transaction of the sequence" $ do
      (vm, env, txs) <- loadReverting
      report <- replay env False vm txs

      report.status @?= "assertion_failed"
      report.transactionCount @?= 3
      map (.index) report.transactions @?= [1, 2, 3]
      map (.status) report.transactions
        @?= ["completed", "reverted", "assertion_failed"]
      map (.result) report.transactions
        @?= ["Stop", "ErrorRevert", "ErrorRevert"]
      assertBool "every transaction reports the gas it burned" $
        all ((> 0) . (.gasUsed)) report.transactions
      assertBool "the call is spelled out" $
        all (T.isInfixOf "assert" . (.call)) report.transactions
      assertBool "the assertion failure shows up in the logs" $
        any (T.isInfixOf "AssertionFailed") (last report.transactions).logs

      -- An assertion failure is what the caller is after, so it is reported
      -- even though the sequence reverted earlier.
      report.failedTxIndex @?= Just 3

  , testCase "counts a failed solidity assert as an assertion failure" $
      withSolcVersion (Just (>= solcV (0,8,0))) $ do
        (vm, env, call) <- load "assert/assert-0.8.sol"
        report <- replay env False vm [call "direct_assert" [AbiInt 256 100]]
        report.status @?= "assertion_failed"
        assertBool "the panic is spelled out" $
          any (T.isInfixOf "Panic(1)") (head report.transactions).logs

  , testCase "leaves the campaign's coverage and corpus alone" $ do
      (vm, env, txs) <- loadReverting
      coverageBefore <- coverageStats env.coverageRefInit env.coverageRefRuntime
      corpusBefore <- corpusSize <$> readIORef env.corpusRef

      _ <- replay env False vm txs

      coverageAfter <- coverageStats env.coverageRefInit env.coverageRefRuntime
      corpusAfter <- corpusSize <$> readIORef env.corpusRef
      coverageAfter @?= coverageBefore
      corpusAfter @?= corpusBefore

  , testCase "includes the EVM trace only when asked" $ do
      (vm, env, txs) <- loadReverting
      without <- replay env False vm txs
      with <- replay env True vm txs
      without.trace @?= Nothing
      assertBool "asking for the trace produces one" (isJust with.trace)
      assertBool "the trace is not coloured" $
        not (T.isInfixOf "\ESC[" (fromMaybe "" with.trace))

  , testCase "reports an empty sequence as completed" $ do
      (vm, env, _) <- loadReverting
      report <- replay env True vm []
      report.status @?= "completed"
      report.transactionCount @?= 0
      report.failedTxIndex @?= Nothing
      assertBool "nothing to report on" (null report.transactions)
      -- Nothing ran, so there is no trace to show even though one was asked for.
      report.trace @?= Nothing
  ]
  where
  -- Compile a fixture and return a way to call functions on it. These fixtures
  -- report failures with events or panics rather than with echidna_ properties,
  -- so they need assertion mode to have any tests at all.
  load :: FilePath -> IO (VM Concrete, Env, Text -> [AbiValue] -> Tx)
  load fixture = do
    let cfg = testConfig
          { solConf = testConfig.solConf { testMode = "assertion" } }
    buildOutput <- compileContracts cfg.solConf (fixture :| [])
    (vm, env, _) <- loadSolTests cfg buildOutput Nothing
    let solConf = env.cfg.solConf
    pure ( vm
         , env
         , \name args ->
             basicTx name args (Set.elemAt 0 solConf.sender) solConf.contractAddr
                     env.cfg.txConf.txGas (0, 0)
         )

  -- A sequence that completes, reverts, and fails an assertion, in that order.
  loadReverting :: IO (VM Concrete, Env, [Tx])
  loadReverting = do
    (vm, env, call) <- load "assert/revert.sol"
    pure ( vm
         , env
         , [ call "assert_revert" [AbiUInt 256 1]
           , call "assert_unreachable" []
           , call "assert_revert" [AbiUInt 256 200]
           ]
         )

  replay :: Env -> Bool -> VM Concrete -> [Tx] -> IO Report
  replay env includeTrace vm txs = do
    json <- runReaderT (executeSeq includeTrace vm txs) env
    either assertFailure pure $ eitherDecodeStrict (encodeUtf8 json)
