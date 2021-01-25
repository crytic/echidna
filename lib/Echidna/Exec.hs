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
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import EVM
import EVM.Op (Op(..))
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Types (Buffer(..))
import EVM.Symbolic (litWord)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Transaction
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
classifyError (Query _)              = RevertE
classifyError StackLimitExceeded     = RevertE
classifyError StackUnderrun          = IllegalE
classifyError BadJumpDestination     = IllegalE
classifyError IllegalOverflow        = IllegalE
classifyError _                      = UnknownE

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

-- | Given an error handler, an execution function, and a transaction, execute that transaction
-- using the given execution strategy, handling errors with the given handler.
execTxWith :: (MonadState x m, Has VM x) => (Error -> m ()) -> m VMResult -> Tx -> m (VMResult, Int)
execTxWith h m t = do
  sd <- hasSelfdestructed (t ^. dst)
  if sd then pure (VMFailure (Revert ""), 0)
  else do 
    hasLens . traces .= emptyEvents 
    (og :: VM) <- use hasLens
    setupTx t
    gasIn <- use $ hasLens . state . gas
    res <- m
    gasOut <- use $ hasLens . state . gas
    cd <- use $ hasLens . state . calldata
    case (res, t ^. call) of
      (f@Reversion, _) -> do
        hasLens .= og
        hasLens . state . calldata .= cd
        hasLens . result ?= f
      (VMFailure x, _) -> h x
      (VMSuccess (ConcreteBuffer bc), SolCreate _) ->
        (hasLens %=) . execState $ do
          env . contracts . at (t ^. dst) . _Just . contractcode .= InitCode ""
          replaceCodeOfSelf (RuntimeCode bc)
          loadContract (t ^. dst)
      _ -> pure ()
    pure (res, fromIntegral (gasIn - gasOut))

-- | Execute a transaction "as normal".
execTx :: (MonadState x m, Has VM x, MonadThrow m) => Tx -> m (VMResult, Int)
execTx = execTxWith vmExcept $ liftSH exec

type CoverageMap = Map BS.ByteString (Set (Int, TxResult))

-- | Given a way of capturing coverage info, execute while doing so once per instruction.
usingCoverage :: (MonadState x m, Has VM x) => m () -> m VMResult
usingCoverage cov = maybe (cov >> liftSH exec1 >> usingCoverage cov) pure =<< use (hasLens . result)

-- | Given good point coverage, count unique points.
coveragePoints :: CoverageMap -> Int
coveragePoints = sum . fmap S.size

-- | Given good point coverage, count the number of unique points but
-- only considering the different instruction PCs (discarding the TxResult).
-- This is useful to report a coverage measure to the user
scoveragePoints :: CoverageMap -> Int
scoveragePoints = sum . fmap (S.size . S.map fst)

-- | Capture the current PC and bytecode (without metadata). This should identify instructions uniquely.
pointCoverage :: (MonadState x m, Has VM x) => Lens' x CoverageMap -> m ()
pointCoverage l = do
  v <- use hasLens
  l %= M.insertWith (const . S.insert $ (v ^. state . pc, Success))
                    (fromMaybe (error "no contract information on coverage") $ h v)
                    mempty
  where
    h v = getBytecodeMetadata <$>
            v ^? env . contracts . at (v ^. state . contract) . _Just . bytecode

traceCoverage :: (MonadState x m, Has VM x, Has [Op] x) => m ()
traceCoverage = do
  v <- use hasLens
  let c = v ^. state . code
  hasLens <>= [readOp (BS.index c $ v ^. state . pc) c]

initialVM :: VM
initialVM = vmForEthrunCreation mempty & block . timestamp .~ litWord initialTimestamp
                                       & block . number .~ initialBlockNumber
                                       & env . contracts .~ mempty       -- fixes weird nonce issues
