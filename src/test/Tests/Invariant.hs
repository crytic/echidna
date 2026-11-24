{-# LANGUAGE GADTs #-}

-- Integration experiment for native properties, using the pinned hevm API.
-- Keep this out of the campaign until transaction semantics and safe state
-- deduplication are settled. In particular, hevm's StateKey omits block context.
-- This experiment uses explicit targets, one sender, zero value and no delay;
-- it never infers a proof from reaching the depth limit.
module Tests.Invariant (invariantTests) where

import Control.Monad (foldM, forM, forM_)
import Control.Monad.Reader (ReaderT, liftIO, runReaderT)
import Control.Monad.ST (stToIO)
import Control.Monad.State.Strict (execStateT)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import EVM (clearTStorages, symbolify)
import EVM.ABI (AbiValue(..), AbiVals(..), Sig(..), decodeBuf, selector)
import EVM.Dapp (DappInfo(..))
import EVM.Effects qualified as HEVM
import EVM.Expr qualified as Expr
import EVM.Fetch qualified as Fetch
import EVM.Invariant (Call(..), InvState(..), Senders(..), Target(..), nextStates, startCall)
import EVM.Solidity (Method(..), SolcContract(..))
import EVM.Solvers (Solver(Bitwuzla), SolverGroup, withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic(StackBased), Postcondition, VeriOpts(..), calldataFromCex, verify)
import EVM.Types (Block(..), Expr(..), Prop(..), ProofResult(..), SMTCex, VM(..), VMResult(..), VMType(..))

import Common (loadSolTests, solcV, testConfig, withSolcVersion)
import Echidna.Exec (execTx)
import Echidna.Solidity (chooseContract, compileContracts)
import Echidna.SymExec.Common (symExecAbstractArith)
import Echidna.SymExec.Symbolic (forceAddr)
import Echidna.Test (CallRes(ResRevert), checkETest, classifyRes, createTest)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..), Env(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestConf(..), TestType(PropertyTest), TestValue(BoolValue))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), basicTx)

data Exploration = Exploration
  { counterexamples :: [(Text, [Tx])]
  , frontierSizes :: [Int]
  , transitionCalls :: Int
  }

invariantTests :: TestTree
invariantTests = testGroup "Native invariant integration experiment"
  [ solidityCase "one call cannot falsify the properties" $ do
      (_, _, result) <- experiment 1
      result.counterexamples @?= []
      result.frontierSizes @?= [1, 1]
      result.transitionCalls @?= 1
  , solidityCase "selected transitions are shared across native properties" $ do
      (vm, env, result) <- experiment 2
      sort (map fst result.counterexamples) @?= ["echidna_not_two", "echidna_revert_expected"]
      result.frontierSizes @?= [1, 1, 1]
      -- Four properties share two target executions. Neither properties nor
      -- the unselected, loop-heavy function become transition targets.
      result.transitionCalls @?= 2
      forM_ result.counterexamples $ \(name, txs) -> do
        map (.call) txs @?= [SolCall ("advance", [AbiUInt 256 1]), SolCall ("advance", [AbiUInt 256 2])]
        endVm <- runReaderT (foldM (\v tx -> snd <$> execTx v tx) vm txs) env
        let test = createTest (PropertyTest name env.cfg.solConf.contractAddr)
        (value, _) <- runReaderT (checkETest test endVm) env
        value @?= BoolValue False
  ]
  where
    solidityCase name = testCase name . withSolcVersion (Just (>= solcV (0,6,9)))

experiment :: Int -> IO (VM Concrete, Env, Exploration)
experiment depth = do
  let cfg = testConfig
        { solConf = testConfig.solConf
            { testMode = "property", disableSlither = True, sender = Set.singleton 0x10000 }
        , testConf = testConfig.testConf { testSender = const 0xcafe }
        , campaignConf = testConfig.campaignConf { symExec = True, workers = Just 0 }
        }
  build <- compileContracts cfg.solConf ("symbolic/native-invariant.sol" :| [])
  (vm, env, _) <- loadSolTests cfg build (Just "NativeInvariant")
  contract <- chooseContract (Map.elems env.dapp.solcByName) (Just "NativeInvariant")
  let selected = [m | m <- Map.elems contract.abiMap, m.methodSignature == "advance(uint256)"]
      targets = [Target (LitAddr cfg.solConf.contractAddr) contract.contractName
                        (Sig m.methodSignature (map snd m.inputs)) | m <- selected]
      properties = [m.name | m <- Map.elems contract.abiMap, cfg.solConf.prefix `T.isPrefixOf` m.name]
      initial = InvState (symbolify vm & #tx % #origin .~ LitAddr 0x10000) []
      runtime = HEVM.defaultEnv { HEVM.config = HEVM.defaultConfig { HEVM.abstractArith = symExecAbstractArith } }
  length targets @?= 1
  length properties @?= 4
  result <- flip runReaderT runtime $
    withSolvers Bitwuzla 1 (Just 30) 1024 $ \solvers ->
      explore env solvers targets properties depth [initial]
  pure (vm, env, result)

type InvariantM = ReaderT HEVM.Env IO

veriOpts :: VeriOpts
veriOpts = VeriOpts
  { iterConf = IterConfig (Just 3) (Just 3) 1 StackBased
  , rpcInfo = Fetch.noRpc
  }

-- Expand a frontier once, then check all properties against the resulting
-- states. Do not put the expansion loop inside the property loop.
explore :: Env -> SolverGroup -> [Target] -> [Text] -> Int -> [InvState] -> InvariantM Exploration
explore env solvers targets properties depth states = do
  failures <- concat <$> forM states (checkProperties env solvers properties)
  if depth == 0 || null states then
    pure $ Exploration failures [length states] 0
  else do
    next <- concat <$> forM states (\st -> do
      let fetcher = Fetch.oracle solvers (Just env.fetchSession) Fetch.noRpc
      (reached, partials, probes) <- nextStates solvers (\_ _ -> PBool True) fetcher
        veriOpts.iterConf env.cfg.txConf.txGas (OneOf [0x10000]) targets st
      liftIO $ length partials @?= 0
      liftIO $ length probes @?= 0
      -- Keep time fixed in this spike using the returned timestamp expression,
      -- without depending on hevm's names for symbolic variables.
      pure [s { vm = s.vm & #constraints %~ (PEq s.vm.block.timestamp st.vm.block.timestamp :) } | s <- reached])
    rest <- explore env solvers targets properties (depth - 1) next
    pure $ Exploration (failures <> rest.counterexamples)
      (length states : rest.frontierSizes)
      (length states * length targets + rest.transitionCalls)

checkProperties :: Env -> SolverGroup -> [Text] -> InvState -> InvariantM [(Text, [Tx])]
checkProperties env solvers properties st = concat <$> forM properties (\name -> do
  let addr = env.cfg.solConf.contractAddr
      caller = LitAddr (env.cfg.testConf.testSender addr)
      fetcher = Fetch.oracle solvers (Just env.fetchSession) Fetch.noRpc
  vm <- liftIO $ stToIO $ flip execStateT (st.vm & #tx % #origin .~ caller) $ do
    clearTStorages
    startCall (LitAddr addr) caller env.cfg.txConf.propGas name (ConcreteBuf (selector (name <> "()")), [])
  (ends, models) <- verify solvers fetcher veriOpts
    (vm & #exploreDepth .~ 0) (propertyPostcondition env.cfg.solConf.prefix name) Nothing
  liftIO $ length [() | Partial {} <- ends] @?= 0
  concat <$> forM models (\case
    Qed -> pure []
    Cex (_, cex) -> do
      txs <- traverse (replayTx env cex) st.calls
      pure [(name, txs)]
    Unknown (reason, _) -> liftIO $ assertFailure $ "inconclusive property check: " <> reason
    Error reason -> liftIO $ assertFailure reason))

propertyPostcondition :: Text -> Text -> Postcondition
propertyPostcondition prefix name _ = \case
  Success _ _ buf _
    | expectsRevert -> PBool False
    | otherwise -> PAnd (PEq (Expr.bufLength buf) (Lit 32)) (PEq (Expr.readWord (Lit 0) buf) (Lit 1))
  Failure _ _ err -> PBool $ expectsRevert && classifyRes (VMFailure err) == ResRevert
  _ -> PBool True
  where expectsRevert = (prefix <> "revert_") `T.isPrefixOf` name

replayTx :: Env -> SMTCex -> Call -> InvariantM Tx
replayTx env cex call = do
  bytes <- calldataFromCex cex call.calldata call.target.sig >>= either (liftIO . assertFailure) pure
  let Sig signature types = call.target.sig
  case decodeBuf types (ConcreteBuf (BS.drop 4 bytes)) of
    (CAbi args, "") -> pure $ basicTx (T.takeWhile (/= '(') signature) args
      (forceAddr call.caller) (forceAddr call.target.addr) env.cfg.txConf.txGas (0, 0)
    _ -> liftIO $ assertFailure "could not decode invariant counterexample"
