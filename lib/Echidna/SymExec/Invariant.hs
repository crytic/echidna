{-# LANGUAGE GADTs #-}

-- | Adapt hevm's invariant transitions to native Echidna properties and
-- assertions, including bounded verification and concrete counterexample replay.
module Echidna.SymExec.Invariant
  ( checkProperty
  , SymbolicSearch(..)
  , exploreSearch
  , invariantSearch
  , searchMethods
  , searchName
  , selectedTarget
  , selectedMethod
  ) where

import Control.Monad (forM, void, when)
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
import EVM.Solidity (Method(..), Mutability(..), SolcContract(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (Postcondition, VeriOpts(..))
import EVM.Types (Expr(..), Prop(..), VMResult(..), VMType(..), EType(End))
import EVM.Types qualified (FrameState(..), VM(..))

import Echidna.ABI (encodeSigWithName)
import Echidna.SymExec.Common (PartialsLogs, TxOrError(..), checkAssertions, checkFoundry, senderConstraints)
import Echidna.Test
  ( CallRes(ResRevert), classifyRes, isFoundryMode, isFoundryVerification
  , isFoundryVerificationMethod, isPropertyMode, isVerificationMode )
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (Filter(..), SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), TestConf(..), TestType(..), isOpen)
import Echidna.Types.Tx (Tx(..), TxCall(NoCall), TxConf(..), basicTx)
import Echidna.Types.Tx qualified as Tx

-- A stateful search shares one hevm frontier across all targets and checks.
data SymbolicSearch
  = SingleMethod Method
  | InvariantSearch [Method] [TestType]
  | BoundedVerification Bool [Method] -- ^ Whether to use Foundry failure semantics.

searchMethods :: SymbolicSearch -> [Method]
searchMethods (SingleMethod method) = [method]
searchMethods (InvariantSearch methods _) = methods
searchMethods (BoundedVerification _ methods) = methods

searchName :: SymbolicSearch -> T.Text
searchName (SingleMethod method) = method.methodSignature
searchName (InvariantSearch methods _) = "invariants over " <> T.intercalate ", " (map (.methodSignature) methods)
searchName (BoundedVerification foundry methods) =
  (if foundry then "bounded Foundry tests over " else "bounded assertions over ")
    <> T.intercalate ", " (map (.methodSignature) methods)

selectedTarget :: [T.Text] -> Method -> Bool
selectedTarget targets method = not ("_no_symexec" `T.isInfixOf` method.name)
  && (null targets || method.name `elem` targets || method.methodSignature `elem` targets)

selectedMethod :: EConfig -> SolcContract -> Method -> Bool
selectedMethod conf contract method = selectedTarget conf.campaignConf.symExecTargets method
  && not (conf.solConf.prefix `T.isPrefixOf` method.name)
  && (not (isFoundryVerification conf.solConf.testMode contract)
      || isFoundryVerificationMethod conf.campaignConf.symExecSeqLen conf.campaignConf.symExecTargets method)
  && case conf.solConf.methodFilter of
    Whitelist names -> sig `elem` names
    Blacklist names -> sig `notElem` names
  where sig = encodeSigWithName contract.contractName (method.name, map snd method.inputs)

-- | Native properties use state-changing transitions and isolated checks.
-- Bounded verification checks assertions in every call, including view/pure
-- methods and no-argument methods needed to reach an assertion.
invariantSearch :: EConfig -> SolcContract -> [EchidnaTest] -> Maybe SymbolicSearch
invariantSearch conf contract tests
  | isVerificationMode conf.solConf.testMode =
      if foundry || conf.campaignConf.symExecSeqLen > 1
        then Just $ BoundedVerification foundry targets
        else Nothing
  | null checks = Nothing
  | otherwise = Just $ InvariantSearch methods checks
  where
    foundry = isFoundryVerification conf.solConf.testMode contract
    checks = [t.testType | t <- tests, isOpen t, eligible t.testType]
    eligible (PropertyTest name _) = not ("_no_symexec" `T.isInfixOf` name)
    eligible AssertionTest{foundry = False, sig = (name, [])} = not ("_no_symexec" `T.isInfixOf` name)
    eligible _ = False
    targets = [m | m <- Map.elems contract.abiMap, selectedMethod conf contract m]
    methods = [m | m <- targets, m.mutability `notElem` [Pure, View]]

exploreSearch :: App m =>
  SymbolicSearch -> EVM.Types.VM Concrete -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)
exploreSearch search vm conf veriOpts solvers rpcInfo session =
  exploreStatefulMethods foundry methods tests depth vm conf veriOpts solvers rpcInfo session
  where
    foundry = isFoundryMode conf.solConf.testMode || case search of
      BoundedVerification enabled _ -> enabled
      _ -> False
    (methods, tests, depth) = case search of
      SingleMethod method -> ([method], [], 1)
      InvariantSearch ms ts -> (ms, ts, conf.campaignConf.symExecSeqLen)
      BoundedVerification _ ms -> (ms, [], conf.campaignConf.symExecSeqLen)

-- hevm owns transaction setup, context bounds, value transfers and model
-- materialization for both single calls and sequences. Echidna supplies test
-- semantics and replays returned transactions through its campaign executor.
exploreStatefulMethods :: App m =>
  Bool -> [Method] -> [TestType] -> Int -> EVM.Types.VM Concrete -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)
exploreStatefulMethods foundry methods tests depth vm conf veriOpts solvers rpcInfo session = do
  exploration <- Driver.newExploration options (symbolify vm)
  -- Assertion-only searches check the transitions themselves; the failing
  -- call counts toward the bound and needs no separate property check.
  when (null tests) $
    void $ Driver.exploreLevel solvers fetcher veriOpts.iterConf exploration depth
  checked <- forM tests $ \test -> do
    result <- Driver.searchInvariant solvers fetcher veriOpts exploration
      (1, depth) (statefulCall conf test)
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
    targets = [Inv.Target (LitAddr conf.solConf.contractAddr) "" (Sig m.name (map snd m.inputs)) | m <- methods]
    post initial result
      | foundry = EVM.Expr.simplifyProp $ foldr PAnd (PBool True)
          [ POr (PNeg $ EVM.Expr.peq (selectorWord initial.state.calldata)
              (selectorWord $ ConcreteBuf $ selector method.methodSignature))
              (checkFoundry method.name initial result)
          | method <- methods ]
      | not (null tests) && isPropertyMode conf.solConf.testMode = PBool True
      | otherwise = checkAssertions [0x1] initial result
    selectorWord = EVM.Expr.readBytes 4 (Lit 0)
    calls = (Inv.defaultCallConfig conf.txConf.txGas)
      { Inv.maxTimeDelay = Just conf.txConf.maxTimeDelay
      , Inv.maxBlockDelay = Just conf.txConf.maxBlockDelay
      , Inv.callerIsOrigin = True
      , Inv.maxValue = if foundry then 0 else conf.txConf.maxValue
      , Inv.valueTransfer = Inv.MintValue
      , Inv.allowNoCalls = not foundry
      }
    options = Driver.ExplorationConfig targets (Inv.OneOf $ Set.toList conf.solConf.sender)
      calls post False
    errors = map (SMTErrorFromResult . T.unpack)
    partials :: [Expr End] -> PartialsLogs
    partials leaves = [formatPartialDetailed Nothing Map.empty p | Partial _ _ p <- leaves]
    replay model calls = Driver.concretizeCalls vm.block model calls <&> \case
      Left err -> SMTErrorFromResult err
      Right concrete -> TxFromResult $ map (\case
        call@Driver.ConcreteNoCall{} -> Tx NoCall 0 0 0 0 0 call.delay
        call@Driver.ConcreteCall{} ->
          let Sig name _ = call.sig
          in (basicTx name call.arguments call.caller call.target conf.txConf.txGas call.delay) { Tx.value = call.value }) concrete

statefulCall :: EConfig -> TestType -> Driver.Check
statefulCall conf test = case test of
  -- The concrete executor checks native properties only after a real call;
  -- NoCall advances context but leaves the VM without a transaction result.
  PropertyTest name addr -> call name addr (LitAddr $ conf.testConf.testSender addr) []
    conf.txConf.propGas (checkProperty conf.solConf.prefix name) False
  AssertionTest{sig = (name, []), addr} -> call name addr caller
    (senderConstraints caller conf.solConf.sender) conf.txConf.txGas (checkAssertions [0x1]) True
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
