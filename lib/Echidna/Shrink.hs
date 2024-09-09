module Echidna.Shrink (shrinkTest) where

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader.Class (MonadReader (ask), asks)
import Control.Monad.State.Strict (MonadIO)
import Control.Monad.ST (RealWorld)
import Data.Set qualified as Set
import Data.List qualified as List
import Data.Maybe (mapMaybe)

import EVM.Types (VM, VMType(..))

import Echidna.Exec
import Echidna.Transaction
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (TestValue(..), EchidnaTest(..), TestState(..), isOptimizationTest)
import Echidna.Types.Tx (Tx(..), hasReverted, isUselessNoCall, catNoCalls, TxCall(..))
import Echidna.Types.Config
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Test (getResultFromVM, checkETest)

 -- | Top level function to shrink the complexity of the sequence of transactions once
shrinkTest
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m)
  => VM Concrete RealWorld
  -> EchidnaTest
  -> m (Maybe EchidnaTest)
shrinkTest vm test = do
  env <- ask
  case test.state of
    -- If we run out of tries to shrink, return the sequence as we have them
    Large i | i >= env.cfg.campaignConf.shrinkLimit && not (isOptimizationTest test) ->
      pure $ Just test { state = Solved }
    Large i ->
      -- Start removing the reverts, if any
      do  repro <- removeReverts vm test.reproducer
          let rr = removeUselessNoCalls $ catNoCalls repro
          -- Check if the sequence can be reduced, in practice this is almost never fails 
          -- since the canShrinkTx function is hard to enforce for all transaction in the sequence 
          if length rr > 1 || any canShrinkTx rr then do
            maybeShrunk <- shrinkSeq vm (checkETest test) test.value rr
            -- check if the shrinked sequence passes the test or not
            pure $ case maybeShrunk of
              -- the test still fails, let's create another test with the reduced sequence
              Just (txs, val, vm') -> do
                Just test { state = Large (i + 1)
                    , reproducer = txs
                    , vm = Just vm'
                    , result = getResultFromVM vm'
                    , value = val }
              Nothing ->
                -- The test passed, so no success with shrinking this time, just bump number of tries to shrink
                Just test { state = Large (i + 1), reproducer = rr}
          else
            pure $ Just test { state = if isOptimizationTest test
                                    then Large (i + 1)
                                    else Solved }
    _ -> pure Nothing

replaceByNoCall :: Tx -> Tx
replaceByNoCall tx = tx { call = NoCall }

-- | Given a sequence of transactions, remove useless NoCalls. These 
-- are when the NoCall have both zero increment in timestamp and block number.
removeUselessNoCalls :: [Tx] -> [Tx]
removeUselessNoCalls = mapMaybe f
  where f tx = if isUselessNoCall tx then Nothing else Just tx 

-- | Given a VM and a sequence of transactions, execute each transaction except the last one.
-- If a transaction reverts, replace it by a "NoCall" with the same parameters as the original call
-- (e.g. same block increment timestamp and number)
removeReverts :: (MonadIO m, MonadReader Env m, MonadThrow m) => VM Concrete RealWorld -> [Tx] -> m [Tx]
removeReverts vm txs = do
  let (itxs, le) = (init txs, last txs)
  ftxs <- removeReverts' vm itxs []
  return (ftxs ++ [le])

removeReverts' :: (MonadIO m, MonadReader Env m, MonadThrow m) => VM Concrete RealWorld -> [Tx] -> [Tx] -> m [Tx]
removeReverts' _ [] ftxs = return $ reverse ftxs
removeReverts' vm (t:txs) ftxs = do
  (_, vm') <- execTx vm t
  if hasReverted vm'
  then removeReverts' vm' txs (replaceByNoCall t: ftxs) 
  else removeReverts' vm' txs (t:ftxs) 

-- | Given a call sequence that solves some Echidna test, try to randomly
-- generate a smaller one that still solves that test.
shrinkSeq
  :: (MonadIO m, MonadRandom m, MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> (VM Concrete RealWorld -> m (TestValue, VM Concrete RealWorld))
  -> TestValue
  -> [Tx]
  -> m (Maybe ([Tx], TestValue, VM Concrete RealWorld))
shrinkSeq vm f v txs = do
  -- apply one of the two possible simplification strategies (shrunk or shorten) with equal probability
  txs' <- uniform =<< sequence [shorten, shrunk]
  -- remove certain type of "no calls"
  let txs'' = removeUselessNoCalls txs' 
  -- check if the sequence still triggers a failed transaction
  (value, vm') <- check txs'' vm
  -- if the test passed it means we didn't shrink successfully (returns Nothing)
  -- otherwise, return a reduced sequence of transaction
  pure $ case (value,v) of
    (BoolValue False, _)              -> Just (txs'', value, vm')
    (IntValue x, IntValue y) | x >= y -> Just (txs'', value, vm')
    _                                 -> Nothing
  where
    check [] vm' = f vm'
    check (x:xs') vm' = do
      (_, vm'') <- execTx vm' x
      check xs' vm''
    -- | Simplify a sequence of transactions reducing the complexity of its arguments (using shrinkTx)
    -- and then reducing its sender (using shrinkSender)
    shrunk = mapM (shrinkSender <=< shrinkTx) txs
    -- | Simplifiy a sequence of transactions randomly dropping one transaction (with uniform selection)
    shorten = (\i -> take i txs ++ drop (i + 1) txs) <$> getRandomR (0, length txs)

-- | Given a transaction, replace the sender of the transaction by another one
-- which is simpler (e.g. it is closer to zero). Usually this means that
-- simplified transactions will try to use 0x10000 as the same caller
shrinkSender :: (MonadReader Env m, MonadRandom m) => Tx -> m Tx
shrinkSender x = do
  senderSet <- asks (.cfg.solConf.sender)
  let orderedSenders = List.sort $ Set.toList senderSet
  case List.elemIndex x.src orderedSenders of
    Just i | i > 0 -> do
      sender <- uniform (take i orderedSenders)
      pure x{src = sender}
    _ -> pure x
