{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.State.Strict (MonadState, execState, get, put)
import Data.Either (isRight)
import Data.Has (Has(..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import EVM
import EVM.Exec (exec)
import EVM.Types (W256(..))

import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Transaction

-- | Broad categories of execution failures: reversions, illegal operations, and ???.
data ErrorClass = RevertE | IllegalE | UnknownE

-- | Given an execution error, classify it. Mostly useful for nice @pattern@s ('Reversion', 'Illegal').
classifyError :: Error -> ErrorClass
classifyError (Revert _)             = RevertE
classifyError (UnrecognizedOpcode _) = RevertE
classifyError (Query _)              = RevertE
classifyError StackUnderrun          = IllegalE
classifyError BadJumpDestination     = IllegalE
classifyError StackLimitExceeded     = IllegalE
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
execTxWith :: (MonadState x m, Has VM x) => (Error -> m ()) -> m VMResult -> Tx -> m VMResult
execTxWith h m t = do og <- get
                      setupTx t
                      res <- m
                      case (res, isRight $ t ^. call) of
                        (Reversion,   _)         -> put og
                        (VMFailure x, _)         -> h x
                        (VMSuccess bc, True) -> hasLens %= execState ( replaceCodeOfSelf bc
                                                                        >> loadContract (t ^. dst))
                        _                        -> pure ()
                      return res

-- | Execute a transaction "as normal".
execTx :: (MonadState x m, Has VM x, MonadThrow m) => Tx -> m VMResult
execTx = execTxWith vmExcept $ liftSH exec

-- | Given a way of capturing coverage info, execute while doing so once per instruction.
usingCoverage :: (MonadState x m, Has VM x) => m () -> m VMResult
usingCoverage cov = maybe (cov >> liftSH exec1 >> usingCoverage cov) pure =<< use (hasLens . result)

-- | Given good point coverage, count unique points.
coveragePoints :: Map W256 (Set Int) -> Int
coveragePoints = sum . fmap S.size

-- | Capture the current PC and codehash. This should identify instructions uniquely (maybe? EVM is weird).
pointCoverage :: (MonadState x m, Has VM x) => Lens' x (Map W256 (Set Int)) -> m ()
pointCoverage l = use hasLens >>= \v ->
  l %= M.insertWith (const . S.insert $ v ^. state . pc) (fromMaybe (W256 maxBound) $ h v) mempty where
    h v = v ^? env . contracts . at (v ^. state . contract) . _Just . codehash
