{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Strict (MonadState(get, put), execState, execState, MonadIO(liftIO), runStateT)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S

import EVM hiding (tx)
import EVM.ABI
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Types (Expr(ConcreteBuf, Lit), hexText)
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Process (readProcessWithExitCode)

import Echidna.Events (emptyEvents)
import Echidna.Transaction
import Echidna.Types (ExecException(..), fromEVM)
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Signature (BytecodeMemo, lookupBytecodeMetadata)
import Echidna.Types.Tx (TxCall(..), Tx, TxResult(..), call, dst, initialTimestamp, initialBlockNumber)

-- | Broad categories of execution failures: reversions, illegal operations, and ???.
data ErrorClass = RevertE | IllegalE | UnknownE

-- | Given an execution error, classify it. Mostly useful for nice @pattern@s ('Reversion', 'Illegal').
classifyError :: Error -> ErrorClass
classifyError (OutOfGas _ _)         = RevertE
classifyError (Revert _)             = RevertE
classifyError (UnrecognizedOpcode _) = RevertE
classifyError StackLimitExceeded     = RevertE
classifyError StackUnderrun          = IllegalE
classifyError BadJumpDestination     = IllegalE
classifyError IllegalOverflow        = IllegalE
classifyError _                      = UnknownE

-- | Extracts the 'Query' if there is one.
getQuery :: VMResult -> Maybe Query
getQuery (VMFailure (Query q)) = Just q
getQuery _                     = Nothing

emptyAccount :: Contract
emptyAccount = initialContract (RuntimeCode (ConcreteRuntimeCode mempty))

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | Given an execution error, throw the appropriate exception.
vmExcept :: MonadThrow m => Error -> m ()
vmExcept e = throwM $ case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e}

-- | Given an error handler `onErr`, an execution strategy `executeTx`, and a transaction `tx`,
-- execute that transaction using the given execution strategy, calling `onErr` on errors.
execTxWith :: (MonadIO m, MonadState s m) => Lens' s VM -> (Error -> m ()) -> m VMResult -> Tx -> m (VMResult, Int)
execTxWith l onErr executeTx tx = do
  vm <- use l
  if hasSelfdestructed vm tx.dst then
    pure (VMFailure (Revert (ConcreteBuf "")), 0)
  else do
    l . traces .= emptyEvents
    vmBeforeTx <- use l
    l %= execState (setupTx tx)
    gasLeftBeforeTx <- use $ l . state . gas
    vmResult <- runFully
    gasLeftAfterTx <- use $ l . state . gas
    handleErrorsAndConstruction vmResult vmBeforeTx
    pure (vmResult, fromIntegral $ gasLeftBeforeTx - gasLeftAfterTx)
  where
  runFully = do
    vmResult <- executeTx
    -- For queries, we halt execution because the VM needs some additional
    -- information from the outside. We provide this information and resume
    -- the execution by recursively calling `runFully`.
    case getQuery vmResult of
      -- A previously unknown contract is required
      Just (PleaseFetchContract _ continuation) -> do
        -- Use the empty contract
        l %= execState (continuation emptyAccount)
        runFully

      -- A previously unknown slot is required
      Just (PleaseFetchSlot _ _ continuation) -> do
        -- Use the zero slot
        l %= execState (continuation 0)
        runFully

      -- Execute a FFI call
      Just (PleaseDoFFI (cmd : args) continuation) -> do
        (_, stdout, _) <- liftIO $ readProcessWithExitCode cmd args ""
        let encodedResponse = encodeAbiValue $
              AbiTuple (V.fromList [AbiBytesDynamic . hexText . T.pack $ stdout])
        l %= execState (continuation encodedResponse)
        runFully

      -- No queries to answer, the tx is fully executed and the result is final
      _ -> pure vmResult

  -- | Handles reverts, failures and contract creations that might be the result
  -- (`vmResult`) of executing transaction `tx`.
  handleErrorsAndConstruction vmResult vmBeforeTx = case (vmResult, tx.call) of
    (Reversion, _) -> do
      tracesBeforeVMReset <- use $ l . traces
      codeContractBeforeVMReset <- use $ l . state . codeContract
      calldataBeforeVMReset <- use $ l . state . calldata
      callvalueBeforeVMReset <- use $ l . state . callvalue
      -- If a transaction reverts reset VM to state before the transaction.
      l .= vmBeforeTx
      -- Undo reset of some of the VM state.
      -- Otherwise we'd loose all information about the reverted transaction like
      -- contract address, calldata, result and traces.
      l . result ?= vmResult
      l . state . calldata .= calldataBeforeVMReset
      l . state . callvalue .= callvalueBeforeVMReset
      l . traces .= tracesBeforeVMReset
      l . state . codeContract .= codeContractBeforeVMReset
    (VMFailure x, _) -> onErr x
    (VMSuccess (ConcreteBuf bytecode'), SolCreate _) ->
      -- Handle contract creation.
      l %= execState (do
        env . contracts . at tx.dst . _Just . contractcode .= InitCode mempty mempty
        replaceCodeOfSelf (RuntimeCode (ConcreteRuntimeCode bytecode'))
        loadContract tx.dst)
    _ -> pure ()

-- | Execute a transaction "as normal".
execTx :: (MonadIO m, MonadState VM m, MonadThrow m) => Tx -> m (VMResult, Int)
execTx = execTxWith id vmExcept $ fromEVM exec

-- | Execute a transaction, logging coverage at every step.
execTxWithCov
  :: (MonadIO m, MonadState VM m, MonadThrow m)
  => BytecodeMemo
  -> Tx
  -> m ((VMResult, Int), CoverageMap)
execTxWithCov memo tx = do
  vm <- get
  (r, (vm', cm)) <- runStateT (execTxWith _1 vmExcept execCov tx) (vm, mempty)
  put vm'
  pure (r, cm)
  where
    -- the same as EVM.exec but collects coverage, will stop on a query
    execCov = do
     (vm, cm) <- get
     let (r, vm', cm') = loop vm cm
     put (vm', cm')
     pure r

    -- | Repeatedly exec a step and add coverage until we have an end result
    loop :: VM -> CoverageMap -> (VMResult, VM, CoverageMap)
    loop vm cm = case vm._result of
      Nothing  -> loop (stepVM vm) (addCoverage vm cm)
      Just r   -> (r, vm, cm)

    -- | Execute one instruction on the EVM
    stepVM :: VM -> VM
    stepVM = execState exec1

    -- | Add current location to the CoverageMap
    addCoverage :: VM -> CoverageMap -> CoverageMap
    addCoverage vm = M.alter
                       (Just . maybe mempty (S.insert $ currentCovLoc vm))
                       (currentMeta vm)

    -- | Get the VM's current execution location
    currentCovLoc vm = (vm._state._pc, fromMaybe 0 $ vmOpIx vm, length vm._frames, Stop)

    -- | Get the current contract's bytecode metadata
    currentMeta vm = fromMaybe (error "no contract information on coverage") $ do
      buffer <- vm ^? env . contracts . at vm._state._contract . _Just . bytecode
      bc <- viewBuffer buffer
      pure $ lookupBytecodeMetadata memo bc

initialVM :: Bool -> VM
initialVM ffi = vmForEthrunCreation mempty
  & block . timestamp .~ Lit initialTimestamp
  & block . number .~ initialBlockNumber
  & env . contracts .~ mempty       -- fixes weird nonce issues
  & allowFFI .~ ffi
