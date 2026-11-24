{-# LANGUAGE GADTs #-}

-- | Adapt hevm's invariant transitions to native Echidna properties and
-- no-argument assertions, including concrete counterexample replay.
module Echidna.SymExec.Invariant
  ( checkProperty
  , exploreMethod
  , statefulTests
  ) where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM)
import Control.Monad.IO.Unlift (liftIO)
import Control.Monad.ST (stToIO)
import Control.Monad.State.Strict (execStateT)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))

import EVM (clearTStorages, symbolify)
import EVM.ABI (AbiVals(..), Sig(..), decodeBuf, selector)
import EVM.Effects (App)
import EVM.Expr qualified
import EVM.Fetch qualified as Fetch
import EVM.Format (formatPartialDetailed)
import EVM.Invariant (startCall)
import EVM.Invariant qualified as Inv
import EVM.Solidity (Method(..), Mutability(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (Postcondition, VeriOpts(..), calldataFromCex, verifyInputsWithHandler)
import EVM.Types (Addr, Block(..), EType(..), Expr(..), ProofResult(..), Prop(..), SMTCex(..), VMResult(..), VMType(..), forceLit, isQed)
import EVM.Types qualified (VM(..))

import Echidna.SymExec.Common (PartialsLogs, TxOrError(..), checkAssertions, exploreSingleMethod, senderConstraints)
import Echidna.Test (CallRes(ResRevert), classifyRes, isPropertyMode, isVerificationMode)
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
  | otherwise = exploreStatefulMethod method tests vm defaultSender conf veriOpts solvers rpcInfo session

data StatefulCall = StatefulCall
  { name :: T.Text
  , addr :: Addr
  , caller :: Expr EAddr
  , constraints :: [Prop]
  , postcondition :: Postcondition
  }

-- One symbolic transition after a concrete corpus prefix, as in the original
-- stateful worker. hevm owns execution and preservation of reached VMs. There
-- is no visited-state pruning here, and no invariant proof at the depth limit.
-- nextStates currently sends zero ETH; symbolic value remains supported by
-- the ordinary single-call assertion/verification path in Common.
-- TODO(hevm): expose call value and transaction context configuration, and
-- return unknown/error results from transition assertion checks. Keep these
-- features upstream instead of reintroducing an Echidna-specific leaf walker.
exploreStatefulMethod :: App m =>
  Method -> [TestType] -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)
exploreStatefulMethod method tests vm defaultSender conf veriOpts solvers rpcInfo session = do
  -- A concrete sender per expansion keeps tx.origin equal to msg.sender,
  -- including account lookup in initTx, without relying on hevm variable names.
  results <- forM (Set.toList conf.solConf.sender) $ \sender -> do
    initial <- liftIO $ stToIO $ execStateT clearTStorages $
      symbolify vm
        & #tx % #origin .~ LitAddr sender
        & #block % #number .~ number
        & #constraints %~ (numberBounds ++)
    let post before leaf
          | isPropertyMode conf.solConf.testMode = PBool True
          | otherwise = case checkAssertions [0x1] False before leaf of
              PBool True -> PBool True
              p -> POr (PNeg $ timeBounds before.block.timestamp) p
    (states, partials, probes) <- Inv.nextStates solvers post fetcher veriOpts.iterConf
      conf.txConf.txGas (Inv.OneOf [sender]) [target] (Inv.InvState initial [])
    failures <- forM probes $ \probe ->
      replay sender probe.state.calls Nothing (Cex probe.cex)
    checked <- forM states $ \st -> do
      let endVm = st.vm & #constraints %~ (timeBounds st.vm.block.timestamp :)
      mconcat <$> forM tests (\test -> do
        let call = statefulCall conf False test
            gas = case test of PropertyTest {} -> conf.txConf.propGas; _ -> conf.txConf.txGas
        testVm <- liftIO $ stToIO $ flip execStateT (endVm & #tx % #origin .~ call.caller) $ do
          clearTStorages
          startCall (LitAddr call.addr) call.caller gas call.name
            (ConcreteBuf (selector (call.name <> "()")), call.constraints)
        abort <- liftIO $ newTVarIO False
        (models, incomplete) <- verifyInputsWithHandler abort solvers veriOpts fetcher
          (testVm & #exploreDepth .~ 0) call.postcondition Nothing
        txs <- traverse (replay sender st.calls (Just test) . fst) $ filter (not . isQed . fst) models
        pure (txs, map (formatPartialDetailed Nothing Map.empty . fst) incomplete))
    pure $ (failures, [formatPartialDetailed Nothing Map.empty p | Partial _ _ p <- partials]) <> mconcat checked
  pure $ mconcat results
  where
    fetcher = Fetch.oracle solvers (Just session) rpcInfo
    target = Inv.Target (LitAddr conf.solConf.contractAddr) "" (Sig method.methodSignature (map snd method.inputs))
    number = Var "invariant_block_number"
    numberBounds = [PGEq number vm.block.number, PLEq (Sub number vm.block.number) (Lit conf.txConf.maxBlockDelay)]
    timeBounds timestamp = PAnd (PGEq timestamp vm.block.timestamp)
      (PLEq (Sub timestamp vm.block.timestamp) (Lit conf.txConf.maxTimeDelay))

    replay sender [call] test (Cex cex) = do
      decoded <- calldataFromCex cex call.calldata call.target.sig
      pure $ case decoded of
        Left err -> SMTErrorFromResult err
        Right bytes -> case decodeBuf (map snd method.inputs) (ConcreteBuf $ BS.drop 4 bytes) of
          (CAbi args, "") ->
            let timestamp = Map.findWithDefault (forceLit vm.block.timestamp) call.timestamp cex.vars
                blockNumber = Map.findWithDefault (forceLit vm.block.number) number cex.vars
                tx = basicTx method.name args sender conf.solConf.contractAddr conf.txConf.txGas
                  (timestamp - forceLit vm.block.timestamp, blockNumber - forceLit vm.block.number)
                check = case test of
                  Just AssertionTest{sig = (name, []), addr} ->
                    let caller = Map.findWithDefault defaultSender (SymAddr "test_caller") cex.addrs
                    in [basicTx name [] caller addr conf.txConf.txGas (0, 0)]
                  _ -> []
            in TxFromResult (tx : check)
          _ -> SMTErrorFromResult "could not decode invariant counterexample"
    replay _ _ _ (Error err) = pure $ SMTErrorFromResult err
    replay _ _ _ (Unknown err) = pure $ SMTErrorFromResult err
    replay _ _ _ _ = pure $ SMTErrorFromResult "unexpected invariant counterexample"

statefulCall :: EConfig -> Bool -> TestType -> StatefulCall
statefulCall conf foundry = \case
  PropertyTest name addr -> StatefulCall
    { name
    , addr
    , caller = LitAddr (conf.testConf.testSender addr)
    , constraints = []
    , postcondition = checkProperty conf.solConf.prefix name
    }
  AssertionTest{sig = (name, []), addr} -> StatefulCall
    { name
    , addr
    , caller = SymAddr "test_caller"
    , constraints = senderConstraints (SymAddr "test_caller") conf.solConf.sender
    , postcondition = checkAssertions [0x1] foundry
    }
  _ -> error "unsupported symbolic stateful test"


-- Match the normal property classifier, including echidna_revert_* properties.
checkProperty :: T.Text -> T.Text -> Postcondition
checkProperty prefix name _ = \case
  Success _ _ buf _
    | expectsRevert -> PBool False
    | otherwise -> PAnd (PEq (EVM.Expr.bufLength buf) (Lit 32)) (PEq (EVM.Expr.readWord (Lit 0) buf) (Lit 1))
  Failure _ _ err -> PBool $ expectsRevert && classifyRes (VMFailure err) == ResRevert
  _ -> PBool True
  where expectsRevert = (prefix <> "revert_") `T.isPrefixOf` name
