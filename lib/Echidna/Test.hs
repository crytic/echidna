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
import Data.Maybe (fromMaybe, fromJust)
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
import Echidna.Types.Test (TestConf(..), CallRes(..), classifyRes)

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> SolTest -> m (Bool, CallRes)
checkETest em t = do
  TestConf p s <- view hasLens
  g <- view (hasLens . propGas)
  vm <- get -- save EVM state
  -- To check these tests, we're going to need a couple auxilary functions:
  --   * matchR[eturn] checks if we just tried to exec 0xfe, which means we failed an assert
  --   * matchC[alldata] checks if we just executed the function we thought we did, based on calldata
  let matchR (Just (VMFailure (UnrecognizedOpcode 0xfe))) = False
      matchR _                                            = True
      matchC sig b = case viewBuffer b of
        Just cd -> not . BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) $ cd
        Nothing -> False
  res <- case t of
    -- If our test is a regular user-defined test, we exec it and check the result
    Left (f, a) -> do
      sd <- hasSelfdestructed a
      (vm',_) <- execTx $ basicTx f [] (s a) a g
      b  <- gets $ p f . getter
      pure (not sd && b, classifyRes vm')
    -- If our test is an auto-generated assertion test, we check if we failed an assert on that fn
    Right sig   -> do
      vm' <- use hasLens
      ret <- matchR <$> use (hasLens . result)
      correctFn <- matchC sig <$> use (hasLens . state . calldata . _1)
      let es = extractEvents em vm'
          fa = null es || not (any (T.isPrefixOf "AssertionFailed(") es)
      pure (correctFn || (ret && fa), classifyRes $ fromJust $ vm' ^. result)
  put vm -- restore EVM state
  pure res

-- | Given a call sequence that solves some Echidna test, try to randomly generate a smaller one that
-- still solves that test.
shrinkSeq :: ( MonadRandom m, MonadReader x m, MonadThrow m
             , Has SolConf x, Has TestConf x, Has TxConf x, MonadState y m
             , Has VM y)
          => m (Bool, CallRes) -> [Tx] -> m [Tx]
shrinkSeq f xs = sequence [shorten, shrunk] >>= uniform >>= ap (fmap . flip bool xs) check where
  check xs' = do
    og <- get
    (res,_) <- traverse_ execTx xs' >> f
    put og
    pure res
  shrinkSender x = do
    l <- view (hasLens . sender)
    case ifind (const (== x ^. src)) l of
      Nothing     -> pure x
      Just (i, _) -> flip (set src) x . fromMaybe (x ^. src) <$> uniformMay (l ^.. folded . indices (< i))
  shrunk = mapM (shrinkSender <=< shrinkTx) xs
  shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)
