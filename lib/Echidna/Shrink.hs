module Echidna.Shrink where

import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, uniformMay)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Strict (MonadState(get, put))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

import EVM (VM)

import Echidna.Exec
import Echidna.Transaction
import Echidna.Events (Events)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestValue(..))
import Echidna.Types.Tx (Tx, TxResult, src)
import Echidna.Types.Config

-- | Given a call sequence that solves some Echidna test, try to randomly generate a smaller one that
-- still solves that test.
shrinkSeq :: (MonadRandom m, MonadReader Env m, MonadThrow m, MonadState VM m)
          => m (TestValue, Events, TxResult) -> (TestValue, Events, TxResult) -> [Tx] -> m ([Tx], TestValue, Events, TxResult)
shrinkSeq f (v,es,r) xs = do
  strategies <- sequence [shorten, shrunk]
  let strategy = uniform strategies
  xs' <- strategy
  (value, events, result) <- check xs'
  -- if the test passed it means we didn't shrink successfully
  pure $ case (value,v) of
    (BoolValue False, _)               ->  (xs', value, events, result)
    (IntValue x, IntValue y) | x >= y  ->  (xs', value, events, result)
    _                                  ->  (xs, v, es, r)
  where
    check xs' = do
      og <- get
      res <- traverse_ execTx xs' >> f
      put og
      pure res
    shrinkSender x = do
      l <- asks (.cfg._sConf._sender)
      case ifind (const (== x ^. src)) l of
        Nothing     -> pure x
        Just (i, _) -> flip (set src) x . fromMaybe (x ^. src) <$> uniformMay (l ^.. folded . indices (< i))
    shrunk = mapM (shrinkSender <=< shrinkTx) xs
    shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)
