{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Control.Lens
import Control.Monad (ap)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Strict (MonadState(..), gets)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Has (Has(..))
import Data.Text (Text)
import EVM
import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Types (Addr)

import Echidna.Exec
import Echidna.Transaction

-- | An Echidna test is just the name of the function to call and the address where its contract is.
type SolTest = (Text, Addr)

-- | Configuration for evaluating Echidna tests.
data TestConf = TestConf { classifier :: VM -> Bool
                           -- ^ Given a VM state, check if a test just passed (typically examing '_result'.)
                         , propSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         , propMaxGas :: Integer
                         , testMaxGas :: Integer
                         }

-- | Possible responses to a call to an Echidna test: @true@, @false@, @REVERT@, and ???.
data CallRes = ResFalse | ResTrue | ResRevert | ResOther deriving (Eq, Show)

-- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess b) | b == encodeAbiValue (AbiBool True)  = ResTrue
                          | b == encodeAbiValue (AbiBool False) = ResFalse
classifyRes Reversion = ResRevert
classifyRes _ = ResOther

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, MonadState y m, Has VM y, MonadThrow m) => SolTest -> m Bool
checkETest (f, a) = asks getter >>= \(TestConf p s g _) -> do
  og <- get 
  res <- execTx g (Tx (Left (f, [])) (s a) a 0) >> gets (p . getter)
  put og
  pure res

-- | Given a call sequence that solves some Echidna test, try to randomly generate a smaller one that
-- still solves that test.
shrinkSeq :: (MonadRandom m, MonadReader x m, Has TestConf x, MonadState y m, Has VM y, MonadThrow m)
          => SolTest -> [Tx] -> m [Tx]
shrinkSeq t xs = sequence [shorten, shrunk] >>= uniform >>= ap (fmap . flip bool xs) check where
  check xs' = do og <- get 
                 g <- testMaxGas <$> view hasLens
                 res <- traverse_ (execTx g) xs' >> checkETest t
                 put og
                 pure res
  shrunk = mapM shrinkTx xs
  shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)
