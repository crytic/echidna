{-# LANGUAGE FlexibleContexts #-}

module Echidna.Shrink where

import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, uniformMay)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(get, put))
import Data.Foldable (traverse_)
import Data.Has (Has(..))
import Data.Maybe (fromMaybe)
import EVM (Error(..), VMResult(..), VM, calldata, result, state)

import Echidna.Exec
import Echidna.Solidity
import Echidna.Transaction
import Echidna.Events (Events)
import Echidna.Types.Test (TestConf(..), EchidnaTest(..), TestValue(..), testType, TestState(..), TestType(..))
import Echidna.Types.Tx (Tx, TxConf, TxResult, basicTx, propGas, src)
import Echidna.Test (getResultFromVM)

-- | Given a call sequence that solves some Echidna test, try to randomly generate a smaller one that
-- still solves that test.
shrinkSeq :: ( MonadRandom m, MonadReader x m, MonadThrow m
             , Has SolConf x, Has TestConf x, Has TxConf x, MonadState y m
             , Has VM y)
          => m (TestValue, Events, TxResult) -> (Events, TxResult) -> [Tx] -> m ([Tx], Events, TxResult)
shrinkSeq f (es,r) xs = do
  strategies <- sequence [shorten, shrunk]
  let strategy = uniform strategies
  xs' <- strategy
  (value, events, result) <- check xs'
  -- if the test passed it means we didn't shrink successfully
  pure $ if (value == BoolValue True) then (xs, es, r) else (xs', events, result)
  where
    check xs' = do
      og <- get
      res <- traverse_ execTx xs' >> f
      put og
      pure res
    shrinkSender x = do
      l <- view (hasLens . sender)
      case ifind (const (== x ^. src)) l of
        Nothing     -> pure x
        Just (i, _) -> flip (set src) x . fromMaybe (x ^. src) <$> uniformMay (l ^.. folded . indices (< i))
    shrunk = mapM (shrinkSender <=< shrinkTx) xs
    shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)
