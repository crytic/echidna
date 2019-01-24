{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Control.Monad (ap)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Strict (MonadState(..), gets)
import Data.Bool (bool)
import Data.Has (Has(..))
import Data.Text (Text)
import EVM
import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Concrete (Blob(..))
import EVM.Types (Addr)

import Echidna.Exec
import Echidna.Transaction

type SolTest = (Text, Addr)

data TestConf = TestConf { classifier :: VM -> Bool
                         , testSender :: Addr -> Addr
                         }

data CallRes = ResFalse | ResTrue | ResRevert | ResOther deriving (Eq, Show)

classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess (B b)) | b == encodeAbiValue (AbiBool True)  = ResTrue
                              | b == encodeAbiValue (AbiBool False) = ResFalse
classifyRes Reversion = ResRevert
classifyRes _ = ResOther

checkETest :: (MonadReader x m, Has TestConf x, MonadState y m, Has VM y, MonadThrow m) => SolTest -> m Bool
checkETest (f, a) = asks getter >>= \(TestConf p s) -> do
  og <- get 
  res <- execTx (Tx (Left (f, [])) (s a) a 0) >> gets (p . getter)
  put og
  pure res

shrinkSeq :: (MonadRandom m, MonadReader x m, Has TestConf x, MonadState y m, Has VM y, MonadThrow m)
          => SolTest -> [Tx] -> m [Tx]
shrinkSeq t xs = shorten >>= mapM shrinkTx >>= ap (fmap . flip bool xs) check where
  check xs' = do {og <- get; res <- traverse execTx xs' >> checkETest t; put og; pure res}
  shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)
