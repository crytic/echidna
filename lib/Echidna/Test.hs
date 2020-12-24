{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad ((<=<), ap)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, uniformMay)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(get, put), gets)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Has (Has(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import EVM (Error(..), VMResult(..), VM, calldata, result, state)
import EVM.ABI (AbiValue(..), abiCalldata, encodeAbiValue)
import EVM.Types (Addr)

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Echidna.ABI
import Echidna.Events (EventMap, extractEvents)
import Echidna.Exec
import Echidna.Solidity
import Echidna.Transaction
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Tx (Tx, TxConf, basicTx, propGas, src)

-- | Configuration for evaluating Echidna tests.
data TestConf = TestConf { classifier :: Text -> VM -> Bool
                           -- ^ Given a VM state and test name, check if a test just passed (typically
                           -- examining '_result'.)
                         , testSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         }

-- | Possible responses to a call to an Echidna test: @true@, @false@, @REVERT@, and ???.
data CallRes = ResFalse | ResTrue | ResRevert | ResOther
  deriving (Eq, Show)

-- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess b) | viewBuffer b == Just (encodeAbiValue (AbiBool True))  = ResTrue
                          | viewBuffer b == Just (encodeAbiValue (AbiBool False)) = ResFalse
                          | otherwise                                             = ResOther
classifyRes Reversion = ResRevert
classifyRes _         = ResOther

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> SolTest -> m Bool
checkETest em t = do
    r <- use (hasLens . result)
    case r of
      Just (VMSuccess _)                         -> checkETest' em t
      Just (VMFailure (UnrecognizedOpcode 0xfe)) -> checkETest' em t
      _                                          -> return True

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest' :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> SolTest -> m Bool
checkETest' em t = do
  TestConf p s <- view hasLens
  -- To check these tests, we're going to need a couple auxilary functions:
  --   * matchR[eturn] checks if we just tried to exec 0xfe, which means we failed an assert
  --   * matchC[alldata] checks if we just executed the function we thought we did, based on calldata
  let matchR (Just (VMFailure (UnrecognizedOpcode 0xfe))) = False
      matchR _                                            = True
      matchC sig b = case viewBuffer b of
        Just cd -> not . BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) $ cd
        Nothing -> False
  vm <- get -- save EVM state
  res <- case t of
    -- If our test is a regular user-defined test, we exec it and check the result
    Left (f, a) -> do
      g <- view (hasLens . propGas)
      sd <- hasSelfdestructed a
      _  <- execTx $ basicTx f [] (s a) a g
      b  <- gets $ p f . getter
      put vm -- restore EVM state
      pure $ not sd && b
    -- If our test is an auto-generated assertion test, we check if we failed an assert on that fn
    Right sig   -> do
      vm' <- use hasLens
      let correctFn = matchC sig $ vm ^. hasLens . state . calldata . _1
          ret = matchR $ vm ^. hasLens . result
          es = extractEvents em vm'
          fa = null es || not (any (T.isPrefixOf "AssertionFailed(") es)
      pure $ correctFn || (ret && fa)
  pure res

-- | Given a call sequence that solves some Echidna test, try to randomly generate a smaller one that
-- still solves that test.
shrinkSeq :: ( MonadRandom m, MonadReader x m, MonadThrow m
             , Has SolConf x, Has TestConf x, Has TxConf x, MonadState y m
             , Has VM y)
          => m Bool -> [Tx] -> m [Tx]
shrinkSeq f xs = sequence [shorten, shrunk] >>= uniform >>= ap (fmap . flip bool xs) check where
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
