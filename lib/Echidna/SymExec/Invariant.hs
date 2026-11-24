{-# LANGUAGE GADTs #-}

-- | Adapt hevm's invariant transitions to native Echidna properties and
-- no-argument assertions, including concrete counterexample replay.
module Echidna.SymExec.Invariant
  ( checkProperty
  , exploreMethod
  , statefulTests
  ) where

import Control.Monad (forM)
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import EVM (symbolify)
import EVM.ABI (Sig(..), selector)
import EVM.Effects (App)
import EVM.Expr qualified
import EVM.Fetch qualified as Fetch
import EVM.Format (formatPartialDetailed)
import EVM.Invariant qualified as Inv
import EVM.Invariant.Driver qualified as Driver
import EVM.Solidity (Method(..), Mutability(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (Postcondition, VeriOpts)
import EVM.Types (Addr, Expr(..), Prop(..), VMResult(..), VMType(..))
import EVM.Types qualified (VM(..))

import Echidna.SymExec.Common (PartialsLogs, TxOrError(..), checkAssertions, exploreSingleMethod, senderConstraints)
import Echidna.Test (CallRes(ResRevert), classifyRes, isPropertyMode, isVerificationMode)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), TestConf(..), TestType(..), isOpen)
import Echidna.Types.Tx (TxConf(..), basicTx)

-- State-changing calls can expose a property or a no-argument assertion failure.
statefulTests :: EConfig -> Method -> [EchidnaTest] -> [TestType]
statefulTests conf method tests
  | isVerificationMode conf.solConf.testMode
    || method.mutability `elem` [Pure, View]
    || conf.solConf.prefix `T.isPrefixOf` method.name
    || "_no_symexec" `T.isInfixOf` method.name = []
  | otherwise = [t.testType | t <- tests, isOpen t, eligible t.testType]
  where
    eligible (PropertyTest n _) = not ("_no_symexec" `T.isInfixOf` n)
    eligible AssertionTest{foundry = False, sig = (n, [])} =
      method.methodSignature /= n <> "()" && not ("_no_symexec" `T.isInfixOf` n)
    eligible _ = False

-- | Check the method alone when there are no eligible stateful tests.
exploreMethod :: App m =>
  Method -> [TestType] -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)
exploreMethod method tests vm defaultSender conf veriOpts solvers rpcInfo session
  | null tests = exploreSingleMethod method vm defaultSender conf veriOpts solvers rpcInfo session
  | otherwise = exploreStatefulMethod method tests vm conf veriOpts solvers rpcInfo session

-- hevm owns the search, VM boundaries and model materialization. Echidna
-- supplies native property semantics and replays returned transactions through
-- its campaign executor for validation, coverage and shrinking. Call value is
-- currently zero; symbolic value belongs in a future hevm API extension.
exploreStatefulMethod :: App m =>
  Method -> [TestType] -> EVM.Types.VM Concrete -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)
exploreStatefulMethod method tests vm conf veriOpts solvers rpcInfo session = do
  exploration <- Driver.newExploration options (symbolify vm)
  checked <- forM tests $ \test -> do
    result <- Driver.searchInvariant solvers fetcher veriOpts exploration
      (1, conf.campaignConf.symExecSeqLen) (statefulCall conf test)
    txs <- case result.counterexample of
      Nothing -> pure []
      Just c -> do
        let check = case test of AssertionTest {} -> [c.call]; _ -> []
        pure <$> replay c.model (c.state.calls <> check)
    pure (txs <> errors result.issues, partials result.partials)
  (_, transitions) <- Driver.explorationStatus exploration
  failures <- forM transitions.probes $ \probe -> replay probe.cex probe.state.calls
  pure $ (failures <> errors transitions.issues, partials transitions.partials) <> mconcat checked
  where
    fetcher = Fetch.oracle solvers (Just session) rpcInfo
    target = Inv.Target (LitAddr conf.solConf.contractAddr) "" (Sig method.name (map snd method.inputs))
    post = if isPropertyMode conf.solConf.testMode then \_ _ -> PBool True else checkAssertions [0x1] False
    options = Driver.ExplorationConfig [target] (Inv.OneOf $ Set.toList conf.solConf.sender)
      (Inv.CallConfig conf.txConf.txGas (Just conf.txConf.maxTimeDelay) (Just conf.txConf.maxBlockDelay) True) post False
    errors = map (SMTErrorFromResult . T.unpack)
    partials leaves = [formatPartialDetailed Nothing Map.empty p | Partial _ _ p <- leaves]
    replay model calls = Driver.concretizeCalls vm.block model calls <&> \case
      Left err -> SMTErrorFromResult err
      Right concrete -> TxFromResult
        [basicTx name call.arguments call.caller call.target conf.txConf.txGas call.delay
        | call <- concrete, let Sig name _ = call.sig]

statefulCall :: EConfig -> TestType -> Driver.Check
statefulCall conf test = case test of
  PropertyTest name addr -> call name addr (LitAddr $ conf.testConf.testSender addr) []
    conf.txConf.propGas (checkProperty conf.solConf.prefix name)
  AssertionTest{sig = (name, []), addr} -> call name addr caller
    (senderConstraints caller conf.solConf.sender) conf.txConf.txGas (checkAssertions [0x1] False)
  _ -> error "unsupported symbolic stateful test"
  where
    caller = SymAddr "test_caller"
    call name addr sender constraints gas = Driver.Check (Inv.Target (LitAddr addr) "" (Sig name []))
      sender (Just sender) gas (ConcreteBuf (selector $ name <> "()"), constraints)

-- Match the normal property classifier, including echidna_revert_* properties.
checkProperty :: T.Text -> T.Text -> Postcondition
checkProperty prefix name _ = \case
  Success _ _ buf _
    | expectsRevert -> PBool False
    | otherwise -> PAnd (PEq (EVM.Expr.bufLength buf) (Lit 32)) (PEq (EVM.Expr.readWord (Lit 0) buf) (Lit 1))
  Failure _ _ err -> PBool $ expectsRevert && classifyRes (VMFailure err) == ResRevert
  _ -> PBool True
  where expectsRevert = (prefix <> "revert_") `T.isPrefixOf` name
