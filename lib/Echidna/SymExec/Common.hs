{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Common where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.ST (stToIO)
import Control.Monad.State.Strict (execState, runStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DoubleWord (Word256)
import Data.List (foldl')
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Optics.Core ((.~), (%), (%~))

import EVM (clearTStorages, loadContract, resetState, symbolify)
import EVM.ABI (Sig(..), decodeBuf, AbiVals(..), selector, encodeAbiValue, AbiValue(..))
import EVM.Effects (TTY, ReadConfig)
import EVM.Expr qualified
import EVM.Fetch qualified as Fetch
import EVM.Format (formatPartialDetailed)
import EVM.Invariant (startCall)
import EVM.Keccak.Preimages qualified as Preimages
import EVM.Solidity (Method(..), Mutability(..))
import EVM.Solvers (SolverGroup, checkSatWithPropsAbortable, withRunAbort)
import EVM.SymExec (mkCalldata, verifyInputsWithHandler, interpretVMAbortable, runExpr, freezeVM, extractProps, VeriOpts(..), subModel, defaultSymbolicValues, Postcondition)
import EVM.Types (Addr, VMType(..), EType(..), EvmError(..), Expr(..), Block(..), W256, SMTCex(..), ProofResult(..), Prop(..), forceLit, isQed)
import qualified EVM.Types (VM(..))

import Echidna.Exec (classifyError, ErrorClass(..))
import Echidna.Test (isFoundryMode, isPropertyMode, isVerificationMode)
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
    eligible (AssertionTest False (n, []) _) =
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

exploreMethod method tests vm defaultSender conf veriOpts solvers rpcInfo session = do
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
  let post = if isPropertyMode conf.solConf.testMode then \_ _ -> PBool True else checkAssertions [0x1] foundry
      toTx = modelToTx dst vm.block.timestamp vm.block.number method conf.solConf.sender defaultSender cd
      logs = map (\(p, _) -> formatPartialDetailed Nothing Map.empty p)
  if null tests then do
    (models, partials) <- verifyInputsWithHandler abort solvers veriOpts fetcher vm'' post Nothing
    pure (map (toTx . fst) $ filter (not . isQed . fst) models, logs partials)
  else do
    -- Keep the complete state, including block context and keccak preimages, for the next call.
    leaves <- withRunAbort solvers abort $ interpretVMAbortable abort fetcher veriOpts.iterConf vm'' runExpr $ \leaf endVm shouldAbort -> do
      result <- case post vm'' leaf of
        PBool True -> pure Qed
        p -> checkSatWithPropsAbortable solvers (Just shouldAbort)
          (PNeg p : extractProps leaf <> Preimages.recordedProps endVm.keccakPreImgs)
      next <- case leaf of
        Success {} -> Just <$> liftIO (stToIO $ freezeVM endVm)
        _ -> pure Nothing
      pure (result, leaf, next)
    checked <- forM leaves $ \(result, leaf, next) -> do
      continuations <- case next of
        Nothing -> pure []
        Just endVm -> forM tests $ \test -> do
          let (testName, addr, caller, props, testPost) = case test of
                PropertyTest n a -> (n, a, LitAddr (conf.testConf.testSender a), [], checkProperty conf.solConf.prefix n)
                AssertionTest _ (n, []) a -> (n, a, SymAddr "test_caller", senderConstraints (SymAddr "test_caller") conf.solConf.sender, checkAssertions [0x1] foundry)
                _ -> error "unsupported symbolic stateful test"
          (_, testVm) <- liftIO $ stToIO $ flip runStateT (endVm & #tx % #origin .~ caller) $ do
            clearTStorages
            startCall (LitAddr addr) caller (fromIntegral conf.txConf.propGas) testName
              (ConcreteBuf (selector (testName <> "()")), props)
          (models, partials) <- verifyInputsWithHandler abort solvers veriOpts fetcher
            (testVm & #exploreDepth .~ 0) testPost Nothing
          let replay res = case (test, res, toTx res) of
                (AssertionTest _ (n, []) a, Cex cex, TxFromResult txs) ->
                  let sender = fromMaybe defaultSender $ Map.lookup (SymAddr "test_caller") cex.addrs
                  in TxFromResult $ txs <> [basicTx n [] sender a maxGasPerBlock (0, 0)]
                _ -> toTx res
          pure (map (replay . fst) $ filter (not . isQed . fst) models, logs partials)
      let partials = case leaf of
            Partial _ _ p -> [formatPartialDetailed Nothing Map.empty p]
            _ -> []
      pure $ ([toTx result | not (isQed result)], partials) <> mconcat continuations
    pure $ mconcat checked

-- Match the normal property classifier, including echidna_revert_* properties.
checkProperty :: T.Text -> T.Text -> Postcondition
checkProperty prefix name _ = \case
  Success _ _ buf _
    | expectsRevert -> PBool False
    | otherwise -> PAnd (PEq (EVM.Expr.bufLength buf) (Lit 32)) (PEq (EVM.Expr.readWord (Lit 0) buf) (Lit 1))
  Failure _ _ err -> PBool $ expectsRevert && case classifyError err of
    RevertE -> True
    _ -> False
  _ -> PBool True
  where expectsRevert = (prefix <> "revert_") `T.isPrefixOf` name
