{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.State.Strict (MonadState, execState)
import Data.Has (Has(..))
import Data.Maybe (fromMaybe)
import EVM
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Types (Buffer(..), SymWord, Word)
import EVM.Symbolic (litWord)

import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Transaction
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Tx (TxCall(..), Tx, TxResult(..), call, dst, initialTimestamp, initialBlockNumber)

import Echidna.Types.Signature (getBytecodeMetadata)
import Echidna.Events (emptyEvents)

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
emptyAccount = initialContract (RuntimeCode mempty)

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | We throw this when our execution fails due to something other than reversion.
data ExecException = IllegalExec Error | UnknownFailure Error

instance Show ExecException where
  show (IllegalExec e) = "VM attempted an illegal operation: " ++ show e
  show (UnknownFailure e) = "VM failed for unhandled reason, " ++ show e
    ++ ". This shouldn't happen. Please file a ticket with this error message and steps to reproduce!"

instance Exception ExecException

-- | Given an execution error, throw the appropriate exception.
vmExcept :: MonadThrow m => Error -> m ()
vmExcept e = throwM $ case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e}

-- | Given an error handler `onErr`, an execution strategy `executeTx`, and a transaction `tx`,
-- execute that transaction using the given execution strategy, calling `onErr` on errors.
execTxWith :: (MonadState x m, Has VM x) => (Error -> m ()) -> m VMResult -> Tx -> m (VMResult, Int)
execTxWith onErr executeTx tx' = do
  isSelfDestruct <- hasSelfdestructed (tx' ^. dst)
  if isSelfDestruct then pure (VMFailure (Revert ""), 0)
  else do
    hasLens . traces .= emptyEvents
    vmBeforeTx <- use hasLens
    setupTx tx'
    gasLeftBeforeTx <- use $ hasLens . state . gas
    vmResult' <- executeTx
    gasLeftAfterTx <- use $ hasLens . state . gas
        -- Continue transaction whose execution queried a contract or slot
    let continueAfterQuery = do
          -- Run remaining effects
          vmResult'' <- executeTx
          -- Correct gas usage
          gasLeftAfterTx' <- use $ hasLens . state . gas
          handleErrorsAndConstruction onErr vmResult'' vmBeforeTx tx'
          return (vmResult'', fromIntegral $ gasLeftBeforeTx - gasLeftAfterTx')
    case getQuery vmResult' of
      -- A previously unknown contract is required
      Just (PleaseFetchContract _ _ continuation) -> do
        -- Use the empty contract
        hasLens %= execState (continuation emptyAccount)
        continueAfterQuery

      -- A previously unknown slot is required
      Just (PleaseFetchSlot _ _ continuation) -> do
        -- Use the zero slot
        hasLens %= execState (continuation 0)
        continueAfterQuery

      -- No queries to answer
      _ -> do
        handleErrorsAndConstruction onErr vmResult' vmBeforeTx tx'
        return (vmResult', fromIntegral $ gasLeftBeforeTx - gasLeftAfterTx)

-- | Handles reverts, failures and contract creations that might be the result
-- (`vmResult`) of executing transaction `tx`.
handleErrorsAndConstruction :: (MonadState s m, Has VM s)
                            => (Error -> m ())
                            -> VMResult
                            -> VM
                            -> Tx
                            -> m ()
handleErrorsAndConstruction onErr vmResult' vmBeforeTx tx' = case (vmResult', tx' ^. call) of
  (Reversion, _) -> do
    tracesBeforeVMReset <- use $ hasLens . traces
    codeContractBeforeVMReset <- use $ hasLens . state . codeContract
    calldataBeforeReset <- use $ hasLens . state . calldata
    -- If a transaction reverts reset VM to state before the transaction.
    hasLens .= vmBeforeTx
    -- Undo reset of some of the VM state.
    -- Otherwise we'd loose all information about the reverted transaction like
    -- contract address, calldata, result and traces.
    hasLens . result ?= vmResult'
    hasLens . state . calldata .= calldataBeforeReset
    hasLens . traces .= tracesBeforeVMReset
    hasLens . state . codeContract .= codeContractBeforeVMReset
  (VMFailure x, _) -> onErr x
  (VMSuccess (ConcreteBuffer bytecode'), SolCreate _) ->
    -- Handle contract creation.
    hasLens %= execState (do
      env . contracts . at (tx' ^. dst) . _Just . contractcode .= InitCode (ConcreteBuffer "")
      replaceCodeOfSelf (RuntimeCode (ConcreteBuffer bytecode'))
      loadContract (tx' ^. dst))
  _ -> pure ()

-- | Execute a transaction "as normal".
execTx :: (MonadState x m, Has VM x, MonadThrow m) => Tx -> m (VMResult, Int)
execTx = execTxWith vmExcept $ liftSH exec

-- | Given a way of capturing coverage info, execute while doing so once per instruction.
usingCoverage :: (MonadState x m, Has VM x) => m () -> m VMResult
usingCoverage cov = maybe (cov >> liftSH exec1 >> usingCoverage cov) pure =<< use (hasLens . result)

-- | Capture the current PC and bytecode (without metadata). This should identify instructions uniquely.
pointCoverage :: (MonadState x m, Has VM x) => Lens' x CoverageMap -> m ()
pointCoverage l = do
  v <- use hasLens
  l %= M.insertWith (const . S.insert $ (v ^. state . pc, fromMaybe 0 $ vmOpIx v, length $ v ^. frames, Stop))
                    (fromMaybe (error "no contract information on coverage") $ h v)
                    mempty
  where
    h v = do
      buffer <- v ^? env . contracts . at (v ^. state . contract) . _Just . bytecode
      bc <- viewBuffer buffer
      pure $ getBytecodeMetadata bc

initialVM :: VM
initialVM = vmForEthrunCreation mempty & block . timestamp .~ litWord initialTimestamp
                                       & block . number .~ initialBlockNumber
                                       & env . contracts .~ mempty       -- fixes weird nonce issues
