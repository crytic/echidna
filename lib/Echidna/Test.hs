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
import EVM (Error(..), VMResult(..), VM, calldata, result, tx, state, substate, selfdestructs)
import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Types (Addr)

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Echidna.ABI
import Echidna.Events (Events, EventMap, extractEvents)
import Echidna.Exec
import Echidna.Transaction
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Test (TestConf(..), EchidnaTest(..), TestMode, testType, TestState(..), TestType(..))
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Tx (Tx, TxConf, basicTx, TxResult(..), getResult, propGas, src)

--- | Possible responses to a call to an Echidna test: @true@, @false@, @REVERT@, and ???.
data CallRes = ResFalse | ResTrue | ResRevert | ResOther
  deriving (Eq, Show)

--- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess b) | viewBuffer b == Just (encodeAbiValue (AbiBool True))  = ResTrue
                          | viewBuffer b == Just (encodeAbiValue (AbiBool False)) = ResFalse
                          | otherwise                                             = ResOther
classifyRes Reversion = ResRevert
classifyRes _         = ResOther


getResultFromVM :: VM -> TxResult
getResultFromVM vm =
  case (vm ^. result) of
    Just r -> getResult r
    Nothing -> error "getResultFromVM failed"

createTest :: TestType -> EchidnaTest
createTest m =  EchidnaTest (Open (-1)) m Stop [] 

createTests :: TestMode -> [Text] -> Addr -> [SolSignature] -> [EchidnaTest]
createTests m ts r ss = case m of
  "exploration" -> [createTest Exploration]
  "property"    -> map (\t -> createTest (PropertyTest t r)) ts ++ [sdt]
  "assertion"   -> (map (\s -> createTest (AssertionTest s r)) $ drop 1 ss) ++ [createTest (CallTest "AssertionFailed(..)" checkAssertionEvent), sdt]
  _             -> error "Invalid test mode"
 where sdt = createTest (CallTest "Target contract is not self-destructed" $ checkSelfDestructedTarget r)
       sdat =  createTest (CallTest "No contract can be self-destructed" $ checkAnySelfDestructed)

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> EchidnaTest -> m (Bool, Events, TxResult)

checkETest em t = case (t ^. testType) of
                  Exploration       -> return (True, [], Stop) -- These values are never used
                  PropertyTest n a  -> checkProperty em (n, a)
                  AssertionTest n a -> checkAssertion em (n, a)
                  CallTest _ f      -> checkCall em f
                  _                 -> error "unhandled test"

checkProperty :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (Text, Addr) -> m (Bool, Events, TxResult)
checkProperty em t = do
    r <- use (hasLens . result)
    case r of
      Just (VMSuccess _) -> checkProperty' em t
      _                  -> return (True, [], Stop) -- These values are never used

-- | Given a property test, evaluate it and see if it currently passes.
checkProperty' :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (Text, Addr) -> m (Bool, Events, TxResult)
checkProperty' em (f,a) = do
  TestConf p s <- view hasLens
  vm <- get -- save EVM state
  -- Our test is a regular user-defined test, we exec it and check the result
  g <- view (hasLens . propGas)
  _  <- execTx $ basicTx f [] (s a) a g (0, 0)
  vm' <- use hasLens
  b  <- gets $ p f . getter
  put vm -- restore EVM state
  pure $ (b, extractEvents em vm', getResultFromVM vm')

checkAssertion :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (SolSignature, Addr) -> m (Bool, Events, TxResult)
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
    vm' <- use hasLens
    let correctFn = matchC s $ vm' ^. state . calldata . _1
        ret = matchR $ vm' ^. result
    pure $ (correctFn || ret, extractEvents em vm', getResultFromVM vm')

checkCall :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (EventMap -> VM -> Bool) -> m (Bool, Events, TxResult)
checkCall em f = do 
  vm <- use hasLens
  pure $ (f em vm, extractEvents em vm, getResultFromVM vm)

checkAssertionEvent :: EventMap -> VM -> Bool
checkAssertionEvent em vm = 
  let es = extractEvents em vm
  in null es || not (any (T.isPrefixOf "AssertionFailed(") es)

checkSelfDestructedTarget :: Addr -> EventMap -> VM -> Bool
checkSelfDestructedTarget a _ vm =
  let sd = vm ^. tx ^. substate ^. selfdestructs 
  in not $ a `elem` sd


checkAnySelfDestructed :: EventMap -> VM -> Bool
checkAnySelfDestructed _ vm =
  let sd = vm ^. tx ^. substate ^. selfdestructs 
  in (length sd) == 0


--checkPanicEvent :: EventMap -> VM -> Bool
--checkPanicEvent em vm = 
--  let es = extractEvents em vm
--  in null es || not (any (T.isPrefixOf "Panic(") es)

--checkErrorEvent :: EventMap -> VM -> Bool
--checkErrorEvent em vm = 
--  let es = extractEvents em vm
--  in null es || not (any (T.isPrefixOf "Error(") es)
