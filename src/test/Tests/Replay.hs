module Tests.Replay (replayTests) where

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (FromJSON(..), eitherDecodeStrict, withObject, (.:), (.:?))
import Data.IORef (readIORef)
import Data.List (intercalate)
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
import Echidna.MCP (concreteTxs)
import Echidna.MCP.Parse (parseFuzzSequence)
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
        case report.transactions of
          [tx] -> assertBool "the panic is spelled out" $
                    any (T.isInfixOf "Panic(1)") tx.logs
          txs -> assertFailure $
                   "expected one transaction, got " <> show (length txs)

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

  , testCase "MCP concrete arguments reach the declared function intact" $
      withSolcVersion (Just (>= solcV (0,8,0))) $ do
        (vm, env, _) <- load "mcp/type-probe.sol"
        let cases =
              [ ("f_none()", ["Seen(8000)"])
              , ("f_uint256(42)", ["Seen(42)"])
              , ("f_uint8(7)", ["Seen(7)"])
              , ("f_uint32(4294967295)", ["Seen(4294967295)"])
              , ("f_int128(-5)", ["SignedSeen(-5)"])
              , ("f_int128(-170141183460469231731687303715884105728)",
                  ["SignedSeen(-170141183460469231731687303715884105728)"])
              , ("f_int128(170141183460469231731687303715884105727)",
                  ["SignedSeen(170141183460469231731687303715884105727)"])
              , ("f_bytes4(0x01020304)", ["Seen(16909060)"])
              , ("f_bytes32(0x" <> replicate 64 'f' <> ")",
                  ["Seen(115792089237316195423570985008687907853269984665640564039457584007913129639935)"])
              , ("f_addr(0x10000)", ["Seen(65536)"])
              , ("f_bool(false)", ["Seen(0)"])
              , ("f_bool(true)", ["Seen(1)"])
              , ("f_uint8s([0, 255])", ["Seen(0)", "Seen(255)"])
              , ("f_int128s([-5, 11])", ["SignedSeen(-5)", "SignedSeen(11)"])
              , ("overloaded(300)", ["Seen(300)"])
              ]
        prototypes <- maybe (assertFailure "Could not parse probe sequence") pure $
          parseFuzzSequence (intercalate ";" (map fst cases))
        txs <- either (assertFailure . T.unpack) pure (concreteTxs env prototypes)
        report <- replay env False vm txs
        report.transactionCount @?= length cases
        map (.status) report.transactions @?= replicate (length cases) "completed"
        forM_ (zip report.transactions cases) $ \(tx, (literal, events)) ->
          forM_ events $ \event -> assertBool (literal <> " emitted " <> show tx.logs) $
            any (T.isInfixOf event) tx.logs

  , testCase "MCP rejects invalid or ambiguous arguments before replay" $
      withSolcVersion (Just (>= solcV (0,8,0))) $ do
        (_, env, _) <- load "mcp/type-probe.sol"
        forM_
          [ ("f_uint8(300)", "300 does not fit in uint8")
          , ("f_uint8(-1)", "-1 does not fit in uint8")
          , ("f_int128(170141183460469231731687303715884105728)", "does not fit in int128")
          , ("f_uint8s([1])", "Expected 2 elements")
          , ("f_uint8s([1,300])", "300 does not fit in uint8")
          , ("f_uint8(true)", "Cannot use true as uint8")
          , ("f_uint8(?)", "Every argument has to be concrete")
          , ("overloaded(7)", "Ambiguous call 'overloaded'")
          ] $ \(literal, expected) -> do
            prototypes <- maybe (assertFailure ("Could not parse " <> literal)) pure $
              parseFuzzSequence literal
            case concreteTxs env prototypes of
              Left err -> assertBool (T.unpack err) (expected `T.isInfixOf` err)
              Right _ -> assertFailure ("Accepted invalid call " <> literal)
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
