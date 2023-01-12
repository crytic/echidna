{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Strict (MonadState (get, put), execState, execState)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Word (Word64)

import EVM
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Types (Expr(ConcreteBuf, Lit))

import Echidna.Events (emptyEvents)
import Echidna.Transaction
import Echidna.Types (ExecException(..), fromEVM)
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Campaign
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
execTxWith :: MonadState s m => Lens' s VM -> (Error -> m ()) -> m VMResult -> Tx -> m (VMResult, Int)
execTxWith l onErr executeTx tx' = do
  vm <- use l
  if hasSelfdestructed vm (tx'^. dst) then
    pure (VMFailure (Revert (ConcreteBuf "")), 0)
  else do
    l . traces .= emptyEvents
    vmBeforeTx <- use l
    l %= execState (setupTx tx')
    gasLeftBeforeTx <- use $ l . state . gas
    vmResult' <- executeTx
    gasLeftAfterTx <- use $ l . state . gas
    checkAndHandleQuery l vmBeforeTx vmResult' onErr executeTx tx' gasLeftBeforeTx gasLeftAfterTx

checkAndHandleQuery :: MonadState s m => Lens' s VM -> VM -> VMResult -> (Error -> m ()) -> m VMResult -> Tx -> Word64 -> Word64 -> m (VMResult, Int)
checkAndHandleQuery l vmBeforeTx vmResult' onErr executeTx tx' gasLeftBeforeTx gasLeftAfterTx =
        -- Continue transaction whose execution queried a contract or slot
    let continueAfterQuery = do
          -- Run remaining effects
          vmResult'' <- executeTx
          -- Correct gas usage
          gasLeftAfterTx' <- use $ l . state . gas
          checkAndHandleQuery l vmBeforeTx vmResult'' onErr executeTx tx' gasLeftBeforeTx gasLeftAfterTx'

    in case getQuery vmResult' of
      -- A previously unknown contract is required
      Just (PleaseFetchContract _ continuation) -> do
        -- Use the empty contract
        l %= execState (continuation emptyAccount)
        continueAfterQuery

      -- A previously unknown slot is required
      Just (PleaseFetchSlot _ _ continuation) -> do
        -- Use the zero slot
        l %= execState (continuation 0)
        continueAfterQuery

      -- No queries to answer
      _ -> do
        handleErrorsAndConstruction l onErr vmResult' vmBeforeTx tx'
        return (vmResult', fromIntegral $ gasLeftBeforeTx - gasLeftAfterTx)

-- | Handles reverts, failures and contract creations that might be the result
-- (`vmResult`) of executing transaction `tx`.
handleErrorsAndConstruction :: MonadState s m
                            => Lens' s VM
                            -> (Error -> m ())
                            -> VMResult
                            -> VM
                            -> Tx
                            -> m ()
handleErrorsAndConstruction l onErr vmResult' vmBeforeTx tx' = case (vmResult', tx' ^. call) of
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
    l . result ?= vmResult'
    l . state . calldata .= calldataBeforeVMReset
    l . state . callvalue .= callvalueBeforeVMReset
    l . traces .= tracesBeforeVMReset
    l . state . codeContract .= codeContractBeforeVMReset
  (VMFailure x, _) -> onErr x
  (VMSuccess (ConcreteBuf bytecode'), SolCreate _) ->
    -- Handle contract creation.
    l %= execState (do
      env . contracts . at (tx' ^. dst) . _Just . contractcode .= InitCode mempty mempty
      replaceCodeOfSelf (RuntimeCode (ConcreteRuntimeCode bytecode'))
      loadContract (tx' ^. dst))
  _ -> pure ()

-- | Execute a transaction "as normal".
execTx :: (MonadState VM m, MonadThrow m) => Tx -> m (VMResult, Int)
execTx = execTxWith id vmExcept $ fromEVM exec

-- | Execute a transaction, logging coverage at every step.
execTxWithCov :: MonadState (VM, Campaign) m => BytecodeMemo -> m VMResult
execTxWithCov memo = do
  (vm, camp) <- get
  let (r, vm', cm') = loop vm camp._coverage
  put (vm', camp { _coverage = cm' })
  pure r
  where
    -- | Repeatedly exec a step and add coverage until we have an end result
    loop :: VM -> CoverageMap -> (VMResult, VM, CoverageMap)
    loop vm cm = case _result vm of
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
    currentCovLoc vm = (vm ^. state . pc, fromMaybe 0 $ vmOpIx vm, length $ vm ^. frames, Stop)

    -- | Get the current contract's bytecode metadata
    currentMeta vm = fromMaybe (error "no contract information on coverage") $ do
      buffer <- vm ^? env . contracts . at (vm ^. state . contract) . _Just . bytecode
      bc <- viewBuffer buffer
      pure $ lookupBytecodeMetadata memo bc

initialVM :: VM
initialVM = vmForEthrunCreation mempty & block . timestamp .~ Lit initialTimestamp
                                       & block . number .~ initialBlockNumber
                                       & env . contracts .~ mempty       -- fixes weird nonce issues
