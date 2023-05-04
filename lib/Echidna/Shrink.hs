module Echidna.Shrink (shrinkTest) where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader.Class (MonadReader (ask), asks)
import Control.Monad.State.Strict (MonadState(get, put), evalStateT, MonadIO)
import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.List qualified as List

import EVM (VM)

import Echidna.Events (extractEvents)
import Echidna.Exec
import Echidna.Transaction
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestValue(..), EchidnaTest(..), TestState(..), isOptimizationTest)
import Echidna.Types.Tx (Tx(..))
import Echidna.Types.Config
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Test (getResultFromVM, checkETest)

shrinkTest
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m)
  => VM
  -> EchidnaTest
  -> m (Maybe EchidnaTest)
shrinkTest vm test = do
  env <- ask
  case test.state of
    Large i | i >= env.cfg.campaignConf.shrinkLimit ->
      pure $ Just test { state = Solved }
    Large i ->
      if length test.reproducer > 1 || any canShrinkTx test.reproducer then do
        maybeShrunk <- evalStateT (shrinkSeq (checkETest test) test.value test.reproducer) vm
        pure $ case maybeShrunk of
          Just (txs, val, vm') -> do
            Just test { state = Large (i + 1)
                 , reproducer = txs
                 , events = extractEvents False env.dapp vm'
                 , result = getResultFromVM vm'
                 , value = val }
          Nothing ->
            -- No success with shrinking this time, just bump trials
            Just test { state = Large (i + 1) }
      else
        pure $ Just test { state = if isOptimizationTest test
                                 then Large (i + 1)
                                 else Solved }
    _ -> pure Nothing

-- | Given a call sequence that solves some Echidna test, try to randomly
-- generate a smaller one that still solves that test.
shrinkSeq
  :: (MonadIO m, MonadRandom m, MonadReader Env m, MonadThrow m, MonadState VM m)
  => m (TestValue, VM)
  -> TestValue
  -> [Tx]
  -> m (Maybe ([Tx], TestValue, VM))
shrinkSeq f v txs = do
  txs' <- uniform =<< sequence [shorten, shrunk]
  (value, vm') <- check txs'
  -- if the test passed it means we didn't shrink successfully
  pure $ case (value,v) of
    (BoolValue False, _)              -> Just (txs', value, vm')
    (IntValue x, IntValue y) | x >= y -> Just (txs', value, vm')
    _                                 -> Nothing
  where
    check xs' = do
      vm <- get
      res <- traverse_ execTx xs' >> f
      put vm
      pure res
    shrunk = mapM (shrinkSender <=< shrinkTx) txs
    shorten = (\i -> take i txs ++ drop (i + 1) txs) <$> getRandomR (0, length txs)

shrinkSender :: (MonadReader Env m, MonadRandom m) => Tx -> m Tx
shrinkSender x = do
  senderSet <- asks (.cfg.solConf.sender)
  let orderedSenders = List.sort $ Set.toList senderSet
  case List.elemIndex x.src orderedSenders of
    Just i | i > 0 -> do
      sender <- uniform (take i orderedSenders)
      pure x{src = sender}
    _ -> pure x
