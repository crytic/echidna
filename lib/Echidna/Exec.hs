{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Ord (comparing)
import Data.Set (Set)
import EVM
import EVM.Concrete (Blob(..))
import EVM.Exec (exec)
import EVM.Types (W256(..))

import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Transaction

data ErrorClass = RevertE | IllegalE | UnknownE

classifyError :: Error -> ErrorClass
classifyError Revert                 = RevertE
classifyError (UnrecognizedOpcode _) = RevertE
classifyError StackUnderrun          = IllegalE
classifyError BadJumpDestination     = IllegalE
classifyError StackLimitExceeded     = IllegalE
classifyError IllegalOverflow        = IllegalE
classifyError _                      = UnknownE

pattern Reversion :: VMResult
pattern Reversion <- VMFailure (classifyError -> RevertE)

pattern Illegal :: VMResult
pattern Illegal <- VMFailure (classifyError -> IllegalE)

data ExecException = IllegalExec Error | UnknownFailure Error

instance Show ExecException where
  show (IllegalExec e) = "VM attempted an illegal operation: " ++ show e
  show (UnknownFailure e) = "VM failed for unhandled reason, " ++ show e
    ++ ". This shouldn't happen. Please file a ticket with this error message and steps to reproduce!"

instance Exception ExecException

vmExcept :: MonadThrow m => Error -> m ()
vmExcept e = throwM $ case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e}

execTxWith :: (MonadState x m, Has VM x) => (Error -> m ()) -> m VMResult -> Tx -> m VMResult
execTxWith h m t = do og <- get
                      setupTx t
                      res <- m
                      case (res, isRight $ t ^. call) of
                        (Reversion,   _)         -> put og
                        (VMFailure x, _)         -> h x
                        (VMSuccess (B bc), True) -> hasLens %= execState ( replaceCodeOfSelf bc
                                                                        >> loadContract (t ^. dst))
                        _                        -> pure ()
                      return res

execTx :: (MonadState x m, Has VM x, MonadThrow m) => Tx -> m VMResult
execTx = execTxWith vmExcept $ liftSH exec

pointCoverage :: (MonadState x m, Has (Map W256 (Set Int)) x, Has VM x) => m ()
pointCoverage = use hasLens >>= \v ->
  hasLens %= M.insertWith (const . S.insert $ v ^. state . pc) (h v) mempty where
    h v = fromMaybe (W256 maxBound) $ v ^? env . contracts . at (v ^. state . contract) . _Just . codehash

fastCoverage :: (MonadState x m, Has (Set Int) x, Has VM x) => m ()
fastCoverage = use hasLens >>= \v -> hasLens %= S.insert (v ^. state . pc)

usingCoverage :: (MonadState x m, Has VM x) => m () -> m VMResult
usingCoverage cov = maybe (cov >> liftSH exec1 >> usingCoverage cov) pure =<< use (hasLens . result)

execTxRecC :: (MonadState x m, Has VM x, Has (Map W256 (Set Int)) x, MonadThrow m) => Tx -> m VMResult
execTxRecC = execTxWith vmExcept (usingCoverage pointCoverage)

coveragePoints :: Map W256 (Set Int) -> Int
coveragePoints = sum . M.map S.size

execTxOptC :: (MonadState x m, Has VM x, Has (Map W256 (Set Int)) x, Has (Set Tx) x, MonadThrow m) => Tx -> m VMResult
execTxOptC t = let hint = id :: Map W256 (Set Int) -> Map W256 (Set Int) in do
  og  <- hasLens <<.= mempty
  res <- execTxRecC t
  new <- M.unionWith S.union og . hint <$> use hasLens
  if comparing coveragePoints new og == GT then hasLens %= S.insert t else pure ()
  return res
