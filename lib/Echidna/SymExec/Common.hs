{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Common where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.ST (stToIO)
import Control.Monad.State.Strict (execState, execStateT, runStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DoubleWord (Word256)
import Data.Function ((&))
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))

import EVM (clearTStorages, loadContract, resetState, symbolify)
import EVM.ABI (Sig(..), decodeBuf, AbiVals(..), selector, encodeAbiValue, AbiValue(..))
import EVM.Effects (App, TTY, ReadConfig)
import EVM.Expr qualified
import EVM.Fetch qualified as Fetch
import EVM.Format (formatPartialDetailed)
import EVM.Invariant (startCall)
import EVM.Invariant qualified as Inv
import EVM.Solidity (Method(..), Mutability(..))
import EVM.Solvers (SolverGroup)
import EVM.SymExec (mkCalldata, verifyInputsWithHandler, calldataFromCex, VeriOpts(..), subModel, defaultSymbolicValues, Postcondition)
import EVM.Types (Addr, VMType(..), VMResult(..), EType(..), EvmError(..), Expr(..), Block(..), W256, SMTCex(..), ProofResult(..), Prop(..), forceLit, isQed)
import qualified EVM.Types (VM(..))

import Echidna.Test (CallRes(ResRevert), classifyRes, isFoundryMode, isPropertyMode, isVerificationMode)
import Echidna.Types (fromEVM)
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestType(..), TestConf(..), EchidnaTest(..), isOpen)
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..), basicTx, maxGasPerBlock)

-- | Enable hevm's arithmetic abstraction for symbolic exploration and verification.
symExecAbstractArith :: Bool
symExecAbstractArith = True

panicMsg :: Word256 -> ByteString
panicMsg err = selector "Panic(uint256)" <> encodeAbiValue (AbiUInt 256 err)

-- | Postcondition for symbolic execution verification.
-- In foundry mode, all reverts are failures except vm.assume failures.
-- In assertion mode, only assertion failures (0xfe opcode, Error(string)
-- "assertion failed", or Panic codes) are detected.
checkAssertions :: [Word256] -> Bool -> Postcondition
checkAssertions errs isFoundry _ vmres
  | isFoundry = case vmres of
      -- vm.assume failures should not be treated as test failures
      Failure _ _ AssumeCheatFailed -> PBool True
      -- All other failures are test failures in foundry mode
      Failure {} -> PBool False
      _ -> PBool True
  | otherwise = case vmres of
      -- Solidity assert() opcode (0xfe)
      Failure _ _ (UnrecognizedOpcode 0xfe) -> PBool False
      -- Concrete revert: check for "assertion failed" message or panic code
      Failure _ _ (Revert (ConcreteBuf msg)) ->
        -- NOTE: assertTrue/assertFalse does not have the double colon after "assertion failed"
        let assertFail = selector "Error(string)" `BS.isPrefixOf` msg
              && "assertion failed" `BS.isPrefixOf` BS.drop txtOffset msg
        in PBool $ not (assertFail || msg `elem` fmap panicMsg errs)
      -- Symbolic revert: check symbolically against panic messages
      -- TODO: also check for Error(string) "assertion failed" in partially-symbolic
      -- buffers, similar to hevm's symbolicFail in EVM.UnitTest
      Failure _ _ (Revert b) ->
        foldl' PAnd (PBool True) (fmap (PNeg . PEq b . ConcreteBuf . panicMsg) errs)
      _ -> PBool True
  where
    txtOffset = 4 + 32 + 32 -- selector + offset + length

type PartialsLogs = [T.Text]

data TxOrError = TxFromResult [Tx] | SMTErrorFromResult String
  deriving (Show)

extractTxs :: [TxOrError] -> [[Tx]]
extractTxs = mapMaybe (\case
  TxFromResult tx -> Just tx
  _ -> Nothing)

extractErrors :: [TxOrError] -> [String]
extractErrors = mapMaybe (\case
  SMTErrorFromResult err -> Just err
  _ -> Nothing)

suitableForSymExec :: Method -> Bool
suitableForSymExec m =
  -- the method must take arguments (otherwise there is nothing to solve for)
  -- and must not opt out via a `_no_symexec` name. Dynamic ABI types
  -- (bytes/string/dynamic arrays) are now supported: hevm concretizes them to a
  -- bounded length (see maxDynSize), so they no longer disqualify a method.
  not (null m.inputs)
  && not (T.isInfixOf "_no_symexec" m.name)

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


blockMakeSymbolic :: Block -> Block
blockMakeSymbolic b
  = b {
      timestamp = Var "symbolic_block_timestamp"
    , number = Var "symbolic_block_number"
  }

addBlockConstraints :: W256 -> W256 -> Block -> [Prop] -> [Prop]
addBlockConstraints maxTimestampDiff maxNumberDiff block cs =
  cs ++ [
    PGEq (Var "symbolic_block_timestamp") (block.timestamp), PLEq (Sub (Var "symbolic_block_timestamp") (block.timestamp)) $ Lit maxTimestampDiff,
    PGEq (Var "symbolic_block_number") (block.number), PLEq (Sub (Var "symbolic_block_number") (block.number)) $ Lit maxNumberDiff
  ]

senderConstraints :: Expr EAddr -> Set Addr -> [Prop]
senderConstraints caller as = [foldr (\a b -> POr b (PEq caller (LitAddr a))) (PBool False) $ Set.toList as]

modelToTx :: Addr -> Expr EWord -> Expr EWord -> Method -> Set Addr -> Addr -> Expr Buf -> ProofResult SMTCex String -> TxOrError
modelToTx dst oldTimestamp oldNumber method senders fallbackSender calldata result =
  case result of
    Cex cex ->
      let
        cd = defaultSymbolicValues $ subModel cex calldata
        types = snd <$> method.inputs
        argdata = case cd of
          Right cd' -> Right $ EVM.Expr.drop 4 (EVM.Expr.simplify cd')
          Left e -> Left e
        args = case argdata of
          Right argdata' -> case decodeBuf types argdata' of
            (CAbi v, _) -> v
            _ -> []
          Left _ -> []

        src_ = fromMaybe 0 $ Map.lookup (SymAddr "caller") cex.addrs
        src = if Set.member src_ senders then src_ else fallbackSender
        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

        -- If symbolic values for delay (timestamp, number) are unconstrained, no delay is applied
        newTimestamp = fromMaybe 0 $ Map.lookup (Var "symbolic_block_timestamp") cex.vars
        diffTimestamp = if newTimestamp == 0 then 0 else newTimestamp - forceLit oldTimestamp

        newNumber = fromMaybe 0 $ Map.lookup (Var "symbolic_block_number") cex.vars
        diffNumber = if newNumber == 0 then 0 else newNumber - forceLit oldNumber

      in TxFromResult [Tx
        { call = SolCall (method.name, args)
        , src = src
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (diffTimestamp, diffNumber)
        }]

    Error err -> SMTErrorFromResult err
    Unknown err -> SMTErrorFromResult err
    r -> error ("Unexpected value in `modelToTx`: " ++ show r)

rpcFetcher :: Functor f =>
  f a -> Maybe W256 -> f (Fetch.BlockNumber, a)

rpcFetcher rpc block = (,) block' <$> rpc
  where block' = maybe Fetch.Latest Fetch.BlockNumber block

removeUnknownResults :: [ProofResult SMTCex String] -> ([ProofResult SMTCex String], [ProofResult SMTCex String])
removeUnknownResults results =
  let
    known = filter (\case Unknown _ -> False ; _ -> True) results
    unknown = filter (\case Unknown _ -> True; _ -> False) results
  in (known, unknown)

getUnknownLogs :: [ProofResult SMTCex String] -> [T.Text]
getUnknownLogs = mapMaybe (\case
  Error err -> Just $ T.pack err
  _ -> Nothing)

exploreMethod :: (MonadUnliftIO m, ReadConfig m, TTY m) =>
  Method -> [TestType] -> EVM.Types.VM Concrete -> Addr -> EConfig -> VeriOpts -> SolverGroup -> Fetch.RpcInfo -> Fetch.Session -> m ([TxOrError], PartialsLogs)

exploreMethod method tests vm defaultSender conf veriOpts solvers rpcInfo session
  | not (null tests) = exploreStatefulMethod method tests vm defaultSender conf veriOpts solvers rpcInfo session
  | otherwise = do
    -- hevm's mkCalldata now returns ((calldata, constraints), caveats); the
    -- caveats (e.g. bounded dynamic args) are not surfaced through echidna yet.
    (calldataSym, _caveats) <- mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
    let
      constraints = snd calldataSym
      cd = fst calldataSym
      fetcher = Fetch.oracle solvers (Just session) rpcInfo
      dst = conf.solConf.contractAddr
    vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
    let
      vm' = vmReset & execState (loadContract (LitAddr dst))
                    & #tx % #isCreate .~ False
                    -- Foundry tests cannot accept ether, force callvalue to zero
                    & #state % #callvalue .~ (if isFoundryMode conf.solConf.testMode then Lit 0 else TxValue)
                    & #state % #caller .~ SymAddr "caller"
                    & #tx % #origin .~ SymAddr "caller"
                    & #state % #calldata .~ cd

      vm'' = symbolify vm'
          & #block %~ blockMakeSymbolic
          & #constraints %~ (addBlockConstraints conf.txConf.maxTimeDelay conf.txConf.maxBlockDelay vm'.block)
          & #constraints %~ (++ constraints ++ senderConstraints (SymAddr "caller") conf.solConf.sender)

    -- TODO we might want to switch vm's state.baseState value to to AbstractBase eventually.
    -- Doing so might mess up concolic execution.
    let foundry = isFoundryMode conf.solConf.testMode
    abort <- liftIO $ newTVarIO False
    (models, partials) <- verifyInputsWithHandler abort solvers veriOpts fetcher vm'' (checkAssertions [0x1] foundry) Nothing
    let results = filter (\(r, _) -> not (isQed r)) models & map fst
    --liftIO $ mapM_ print partials
    return (map (modelToTx dst vm.block.timestamp vm.block.number method conf.solConf.sender defaultSender cd) results, map (\(p, _) -> formatPartialDetailed Nothing Map.empty p) partials)

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
-- the ordinary single-call assertion/verification path above.
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
