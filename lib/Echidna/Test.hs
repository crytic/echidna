{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad ((<=<), ap)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, uniformMay)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Strict (MonadState(..), gets)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Has (Has(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import EVM (VMResult(..), VM)
import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Types (Addr)

import Echidna.Exec
import Echidna.Solidity
import Echidna.Transaction

-- | An Echidna test is just the name of the function to call and the address where its contract is.
type SolTest = (Text, Addr)

-- | Configuration for evaluating Echidna tests.
data TestConf = TestConf { classifier :: Text -> VM -> Bool
                           -- ^ Given a VM state and test name, check if a test just passed (typically
                           -- examining '_result'.)
                         , testSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         }

-- | Possible responses to a call to an Echidna test: @true@, @false@, @REVERT@, and ???.
data CallRes = ResFalse | ResTrue | ResRevert | ResOther deriving (Eq, Show)

-- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess b) | b == encodeAbiValue (AbiBool True)  = ResTrue
                          | b == encodeAbiValue (AbiBool False) = ResFalse
                          | otherwise                           = ResOther

classifyRes Reversion = ResRevert
classifyRes _ = ResOther

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m) => SolTest -> m Bool
checkETest (f, a) = asks getter >>= \(TestConf p s) -> view (hasLens . propGas) >>= \g -> do
  og <- get 
  res <- execTx (Tx (Left (f, [])) (s a) a g 0) >> gets (p f . getter)
  put og
  pure res

-- | Given a call sequence that solves some Echidna test, try to randomly generate a smaller one that
-- still solves that test.
shrinkSeq :: ( MonadRandom m, MonadReader x m, MonadThrow m
             , Has SolConf x, Has TestConf x, Has TxConf x, MonadState y m, Has VM y)
          => SolTest -> [Tx] -> m [Tx]
shrinkSeq t xs = sequence [shorten, shrunk] >>= uniform >>= ap (fmap . flip bool xs) check where
  check xs' = do {og <- get; res <- traverse_ execTx xs' >> checkETest t; put og; pure res}
  shrinkSender x = view (hasLens . sender) >>= \l -> case ifind (const (== x ^. src)) l of
    Nothing     -> pure x
    Just (i, _) -> flip (set src) x . fromMaybe (x ^. src) <$> uniformMay (l ^.. folded . indices (< i))
  shrunk = mapM (shrinkSender <=< shrinkTx) xs
  shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)
