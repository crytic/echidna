{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Strict (MonadState, execState)
import Data.Either (isRight)
import Data.Has (Has(..))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import EVM
import EVM.Op (Op(..))
import EVM.Exec (exec)
import EVM.Types (W256(..))

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Transaction
import Echidna.Types (Tx, ExecException(..), call, dst)
import Echidna.Util (pattern Illegal, pattern Reversion)

-- | Given an execution error, throw the appropriate exception.
vmExcept :: MonadThrow m => Error -> m ()
vmExcept e = throwM $ case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e}

-- | Given an error handler, an execution function, and a transaction, execute that transaction
-- using the given execution strategy, handling errors with the given handler.
execTxWith :: (MonadState x m, Has VM x) => (Error -> m ()) -> m VMResult -> Tx -> m VMResult
execTxWith h m t = do (og :: VM) <- use hasLens
                      setupTx t
                      res <- m
                      cd  <- use $ hasLens . state . calldata
                      case (res, isRight $ t ^. call) of
                        (f@Reversion, _)         -> do hasLens .= og
                                                       hasLens . state . calldata .= cd
                                                       hasLens . result ?= f
                        (VMFailure x, _)         -> h x
                        (VMSuccess bc, True)     -> (hasLens %=) . execState $ do
                          env . contracts . at (t ^. dst) . _Just . contractcode .= InitCode ""
                          replaceCodeOfSelf (RuntimeCode bc)
                          loadContract (t ^. dst)
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

traceCoverage :: (MonadState x m, Has VM x, Has [Op] x) => m ()
traceCoverage = use hasLens >>= \v -> let c = v ^. state . code in hasLens <>= [readOp (BS.index c $ v ^. state . pc) c]
