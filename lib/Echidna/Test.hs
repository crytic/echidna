{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(get, put), gets)
import Data.Has (Has(..))
import Data.Text (Text)
import EVM (Error(..), VMResult(..), VM, calldata, codeContract, result, tx, state, substate, selfdestructs)
import EVM.ABI (AbiValue(..), AbiType(..), encodeAbiValue, decodeAbiValue, )
import EVM.Types (Addr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T

import Echidna.ABI
import Echidna.Events (Events, EventMap, extractEvents)
import Echidna.Exec
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Test
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Tx (Tx, TxConf, basicTx, TxResult(..), getResult, propGas)

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
  case vm ^. result of
    Just r -> getResult r
    Nothing -> error "getResultFromVM failed"

createTest :: TestType -> EchidnaTest
createTest m =  EchidnaTest (Open (-1)) m v [] Stop []   
                where v = case m of 
                           PropertyTest _ _     -> BoolValue True
                           OptimizationTest _ _ -> IntValue minBound
                           _                    -> NoValue

--assertPanicTest :: EchidnaTest
--assertPanicTest = createTest $ CallTest "Assertion failure detector" (checkPanicEvent "1")

--integerOverflowTest :: EchidnaTest
--integerOverflowTest = createTest $ CallTest "Integer overflow detector" (checkPanicEvent "17")

isAssertionMode :: TestMode -> Bool
isAssertionMode "assertion" = True
isAssertionMode _           = False

isExplorationMode :: TestMode -> Bool
isExplorationMode "exploration" = True
isExplorationMode _             = False

isPropertyMode :: TestMode -> Bool
isPropertyMode "property" = True
isPropertyMode _          = False

createTests :: TestMode -> Bool -> [Text] -> Addr -> [SolSignature] -> [EchidnaTest]
createTests m td ts r ss = case m of
  "exploration"  -> [createTest Exploration]
  "overflow"     -> [createTest (CallTest "Integer (over/under)flow" checkOverflowTest)]
  "property"     -> map (\t -> createTest (PropertyTest t r)) ts
  "optimization" -> map (\t -> createTest (OptimizationTest t r)) ts
  "assertion"    -> map (\s -> createTest (AssertionTest s r)) (filter (/= fallback) ss) ++ [createTest (CallTest "AssertionFailed(..)" checkAssertionTest)]
  _              -> error "Invalid test mode"
 ++ (if td then [sdt, sdat] else [])
   where sdt  = createTest (CallTest "Target contract is not self-destructed" $ checkSelfDestructedTarget r)
         sdat = createTest (CallTest "No contract can be self-destructed" checkAnySelfDestructed)

updateOpenTest :: EchidnaTest -> [Tx] -> Int -> (TestValue, Events, TxResult) -> EchidnaTest
updateOpenTest test txs _ (BoolValue False,es,r) = test { _testState = Large (-1), _testReproducer = txs, _testEvents = es, _testResult = r } 
updateOpenTest test _   i (BoolValue True,_,_)   = test { _testState = Open (i + 1) } 


updateOpenTest test txs i (IntValue v',es,r) = if v' > v then test { _testState = Open (i + 1), _testReproducer = txs, _testValue = IntValue v', _testEvents = es, _testResult = r } 
                                                         else test { _testState = Open (i + 1) }
                                                where v = case test ^. testValue of
                                                           IntValue x -> x
                                                           _          -> error "Invalid type of value for optimization" 


updateOpenTest _ _ _ _                       = error "Invalid type of test"

--updateLargeTest :: EchidnaTest -> EchidnaTest
--updateLargeTest test i = if length x > 1 || any canShrinkTx x
--                             then do (txs, val, evs, r) <- evalStateT (shrinkSeq (checkETest em test) (v, es, res) x) vm
--                                     pure $ test { _testState = Large (i + 1), _testReproducer = txs, _testEvents = evs, _testResult = r, _testValue = val} 
--                             else pure $ test { _testState = Solved, _testReproducer = x}
--                             t 


-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> EchidnaTest -> m (TestValue, Events, TxResult)
checkETest em t = case t ^. testType of
                  Exploration           -> return (BoolValue True, [], Stop) -- These values are never used
                  PropertyTest n a      -> checkProperty em (n, a)
                  OptimizationTest n a  -> checkOptimization em (n, a) 
                  AssertionTest n a     -> checkAssertion em (n, a)
                  CallTest _ f          -> checkCall em f

checkProperty :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (Text, Addr) -> m (TestValue, Events, TxResult)
checkProperty em t = do
    r <- use (hasLens . result)
    case r of
      Just (VMSuccess _) -> checkProperty' em t
      _                  -> return (BoolValue True, [], Stop) -- These values are never used


runTx :: (MonadReader x m, MonadState y m, Has VM y, Has TxConf x, MonadThrow m)
           => Text -> (Addr -> Addr) -> Addr -> m (y, VM)
runTx f s a = do
  vm <- get -- save EVM state
  -- Our test is a regular user-defined test, we exec it and check the result
  g <- view (hasLens . propGas)
  _  <- execTx $ basicTx f [] (s a) a g (0, 0)
  vm' <- use hasLens
  return (vm, vm')


-- | Given a property test, evaluate it and see if it currently passes.
checkProperty' :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (Text, Addr) -> m (TestValue, Events, TxResult)
checkProperty' em (f,a) = do
  TestConf p s <- view hasLens
  (vm, vm') <- runTx f s a
  b  <- gets $ p f . getter
  put vm -- restore EVM state
  pure (BoolValue b, extractEvents em vm', getResultFromVM vm')

--- | Extract a test value from an execution.
getIntFromResult :: Maybe VMResult -> TestValue
getIntFromResult (Just (VMSuccess b)) = case viewBuffer b of
                           Nothing -> error "invalid decode of buffer"
                           Just bs -> case decodeAbiValue (AbiIntType 256) $ LBS.fromStrict bs of
                                        AbiInt 256 n -> IntValue n
                                        _            -> error ("invalid decode of int256: " ++ show bs)
getIntFromResult _ = IntValue minBound

-- | Given a property test, evaluate it and see if it currently passes.
checkOptimization :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (Text, Addr) -> m (TestValue, Events, TxResult)
checkOptimization em (f,a) = do
  TestConf _ s <- view hasLens
  (vm, vm') <- runTx f s a
  put vm -- restore EVM state
  pure (getIntFromResult (vm' ^. result), extractEvents em vm', getResultFromVM vm')


checkAssertion :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (SolSignature, Addr) -> m (TestValue, Events, TxResult)
checkAssertion eventMap (sig, addr) = do
  vm <- use hasLens
      -- Whether the last transaction called the function `sig`.
  let isCorrectFn = case viewBuffer $ vm ^. state . calldata . _1 of
        Just cd -> BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) cd
        Nothing -> False 
      -- Whether the last transaction executed a function on the contract `addr`.
      isCorrectAddr = addr == vm ^. state . codeContract
      isCorrectTarget = isCorrectFn && isCorrectAddr
      -- Whether the last transaction executed opcode 0xfe, meaning an assertion failure.
      isAssertionFailure = case vm ^. result of
        Just (VMFailure (UnrecognizedOpcode 0xfe)) -> True
        _ -> False
      -- Test always passes if it doesn't target the last executed contract and function.
      -- Otherwise it passes if it doesn't cause an assertion failure.
      es = extractEvents eventMap vm
      eventFailure = not (null es) && (checkAssertionEvent es || checkPanicEvent "1" es)
      isFailure = isCorrectTarget && (eventFailure || isAssertionFailure)
  pure (BoolValue (not isFailure), es, getResultFromVM vm)

checkCall :: (MonadReader x m, Has TestConf x, Has TxConf x, MonadState y m, Has VM y, MonadThrow m)
           => EventMap -> (EventMap -> VM -> TestValue) -> m (TestValue, Events, TxResult)
checkCall em f = do 
  vm <- use hasLens
  pure (f em vm, extractEvents em vm, getResultFromVM vm)

checkAssertionTest :: EventMap -> VM -> TestValue
checkAssertionTest em vm = 
  let es = extractEvents em vm
  in BoolValue $ null es || not (checkAssertionEvent es)

checkAssertionEvent :: Events -> Bool
checkAssertionEvent = any (T.isPrefixOf "AssertionFailed(")

checkSelfDestructedTarget :: Addr -> EventMap -> VM -> TestValue
checkSelfDestructedTarget a _ vm =
  let sd = vm ^. (tx . substate . selfdestructs)
  in BoolValue $ a `notElem` sd

checkAnySelfDestructed :: EventMap -> VM -> TestValue
checkAnySelfDestructed _ vm =
  let sd = vm ^. (tx . substate . selfdestructs)
  in BoolValue $ null sd

checkPanicEvent :: T.Text -> Events -> Bool
checkPanicEvent n = any (T.isPrefixOf ("Panic(" <> n <> ")"))

checkOverflowTest :: EventMap -> VM -> TestValue
checkOverflowTest em vm = 
  let es = extractEvents em vm
  in BoolValue $ null es || not (checkPanicEvent "17" es)

--checkErrorEvent :: EventMap -> VM -> Bool
--checkErrorEvent em vm = 
--  let es = extractEvents em vm
--  in null es || not (any (T.isPrefixOf "Error(") es)
