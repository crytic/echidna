{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, uniformMay)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(get, put), gets)
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
import Echidna.Transaction
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Test (TestConf(..), EchidnaTest(..), testType, TestState(..), TestType(..))
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Tx (Tx, TxConf, basicTx, propGas, src)

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
           => EventMap -> EchidnaTest -> m Bool

checkETest em t = case (t ^. testType) of
                  Exploration       -> return True
                  PropertyTest n a  -> checkProperty (n, a)
                  AssertionTest n a -> checkAssertion em (n, a)
                  CallTest _ f      -> checkCall em f
                  _                 -> error "unhandled test"

checkProperty :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => (Text, Addr) -> m Bool
checkProperty t = do
    r <- use (hasLens . result)
    case r of
      Just (VMSuccess _) -> checkProperty' t
      _                  -> return True

-- | Given a property test, evaluate it and see if it currently passes.
checkProperty' :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => (Text, Addr) -> m Bool
checkProperty' (f,a) = do
  TestConf p s <- view hasLens
  vm <- get -- save EVM state
  -- Our test is a regular user-defined test, we exec it and check the result
  g <- view (hasLens . propGas)
  sd <- hasSelfdestructed a
  _  <- execTx $ basicTx f [] (s a) a g (0, 0)
  b  <- gets $ p f . getter
  put vm -- restore EVM state
  pure $ not sd && b

checkAssertion :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (SolSignature, Addr) -> m Bool
checkAssertion em (s, _) =
  -- To check these tests, we're going to need a couple auxilary functions:
  --   * matchR[eturn] checks if we just tried to exec 0xfe, which means we failed an assert
  --   * matchC[alldata] checks if we just executed the function we thought we did, based on calldata
  let matchR (Just (VMFailure (UnrecognizedOpcode 0xfe))) = False
      matchR _                                            = True
      matchC sig b = case viewBuffer b of
        Just cd -> not . BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) $ cd
        Nothing -> False 
  in do
    vm <- use hasLens
    let correctFn = matchC s $ vm ^. state . calldata . _1
        ret = matchR $ vm ^. result
    pure $ correctFn || ret

checkCall :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (EventMap -> VM -> Bool) -> m Bool
checkCall em f = do 
  vm <- use hasLens
  pure $ f em vm

checkAssertionEvent :: EventMap -> VM -> Bool
checkAssertionEvent em vm = 
  let es = extractEvents em vm
  in null es || not (any (T.isPrefixOf "AssertionFailed(") es)
