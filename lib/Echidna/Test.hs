{-# LANGUAGE FlexibleContexts #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(get, put), gets)
import Data.Has (Has(..))
import Data.Text (Text, isPrefixOf)
import EVM (Error(..), VMResult(..), VM, calldata, callvalue, codeContract, result, tx, state, substate, selfdestructs)
import EVM.ABI (AbiValue(..), AbiType(..), encodeAbiValue, decodeAbiValue, )
import EVM.Types (Addr)
import EVM.Symbolic (forceLit)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Text as T

import Echidna.ABI
import Echidna.Events (Events, extractEvents)
import Echidna.Exec
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Test
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Tx (Tx, TxConf, basicTx, TxResult(..), getResult, propGas)

import EVM.Dapp (DappInfo)

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

validateTestModeError :: String
validateTestModeError = "Invalid test mode (should be property, assertion, optimization, overflow or exploration)"

validateTestMode :: String -> TestMode
validateTestMode s = case s of
  "property"     -> s
  "assertion"    -> s
  "stateless"    -> s
  "exploration"  -> s
  "overflow"     -> s
  "optimization" -> s
  _              -> error validateTestModeError

isAssertionMode :: TestMode -> Bool
isAssertionMode "assertion" = True
isAssertionMode _           = False

isExplorationMode :: TestMode -> Bool
isExplorationMode "exploration" = True
isExplorationMode _             = False

isPropertyMode :: TestMode -> Bool
isPropertyMode "property" = True
isPropertyMode _          = False

isStatelessMode :: TestMode -> Bool
isStatelessMode "stateless" = True
isStatelessMode _           = False

createTests :: TestMode -> Bool -> [Text] -> Addr -> [SolSignature] -> [EchidnaTest]
createTests m td ts r ss = case m of
  "exploration"  -> [createTest Exploration]
  "overflow"     -> [createTest (CallTest "Integer (over/under)flow" checkOverflowTest)]
  "property"     -> map (\t -> createTest (PropertyTest t r)) ts
  "optimization" -> map (\t -> createTest (OptimizationTest t r)) ts
  "assertion"    -> map (\s -> createTest (AssertionTest False s r)) (filter (/= fallback) ss) ++ [createTest (CallTest "AssertionFailed(..)" checkAssertionTest)]
  "stateless"    -> map (\s -> createTest (AssertionTest True s r)) (filter (\(_, xs) -> length xs > 0) ss)
  _              -> error validateTestModeError

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

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => EchidnaTest -> m (TestValue, Events, TxResult)
checkETest test = case test ^. testType of
                  Exploration           -> return (BoolValue True, [], Stop) -- These values are never used
                  PropertyTest n a      -> checkProperty (n, a)
                  OptimizationTest n a  -> checkOptimization (n, a)
                  AssertionTest st n a  -> if st then checkStatelessAssertion (n, a) else checkStatefullAssertion (n, a)
                  CallTest _ f          -> checkCall f

checkProperty :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => (Text, Addr) -> m (TestValue, Events, TxResult)
checkProperty t = do
    r <- use (hasLens . result)
    case r of
      Just (VMSuccess _) -> checkProperty' t
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
checkProperty' :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => (Text, Addr) -> m (TestValue, Events, TxResult)
checkProperty' (f,a) = do
  dappInfo <- view hasLens
  TestConf p s <- view hasLens
  (vm, vm') <- runTx f s a
  b  <- gets $ p f . getter
  put vm -- restore EVM state
  pure (BoolValue b, extractEvents dappInfo vm', getResultFromVM vm')

--- | Extract a test value from an execution.
getIntFromResult :: Maybe VMResult -> TestValue
getIntFromResult (Just (VMSuccess b)) = case viewBuffer b of
                           Nothing -> error "invalid decode of buffer"
                           Just bs -> case decodeAbiValue (AbiIntType 256) $ LBS.fromStrict bs of
                                        AbiInt 256 n -> IntValue n
                                        _            -> error ("invalid decode of int256: " ++ show bs)
getIntFromResult _ = IntValue minBound

-- | Given a property test, evaluate it and see if it currently passes.
checkOptimization :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => (Text, Addr) -> m (TestValue, Events, TxResult)
checkOptimization (f,a) = do
  TestConf _ s <- view hasLens
  dappInfo <- view hasLens
  (vm, vm') <- runTx f s a
  put vm -- restore EVM state
  pure (getIntFromResult (vm' ^. result), extractEvents dappInfo vm', getResultFromVM vm')


checkStatefullAssertion :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => (SolSignature, Addr) -> m (TestValue, Events, TxResult)
checkStatefullAssertion (sig, addr) = do
  dappInfo <- view hasLens
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
      events = extractEvents dappInfo vm
      eventFailure = not (null events) && (checkAssertionEvent events || checkPanicEvent "1" events)
      isFailure = isCorrectTarget && (eventFailure || isAssertionFailure)
  pure (BoolValue (not isFailure), events, getResultFromVM vm)


checkStatelessAssertion :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => (SolSignature, Addr) -> m (TestValue, Events, TxResult)
checkStatelessAssertion (sig, addr) = do
  dappInfo <- view hasLens
  vm <- use hasLens
  -- Whether the last transaction has any value
  let hasValue = (forceLit $ vm ^. state . callvalue) /= 0
      -- Whether the last transaction called the function `sig`.
  let isCorrectFn = case viewBuffer $ vm ^. state . calldata . _1 of
        Just cd -> BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) cd
        Nothing -> False 
      isAssertionFailure = case vm ^. result of
        Just (VMFailure _) -> True
        _                  -> False
      events = extractEvents dappInfo vm 
      isFailure = (not hasValue || isAssertionFailure)
  pure (BoolValue (not isFailure), events, getResultFromVM vm)


checkCall :: (MonadReader x m, Has TestConf x, Has TxConf x, Has DappInfo x, MonadState y m, Has VM y, MonadThrow m)
           => (DappInfo -> VM -> TestValue) -> m (TestValue, Events, TxResult)
checkCall f = do
  dappInfo <- view hasLens
  vm <- use hasLens
  pure (f dappInfo vm, extractEvents dappInfo vm, getResultFromVM vm)

checkAssertionTest :: DappInfo -> VM -> TestValue
checkAssertionTest dappInfo vm = 
  let events = extractEvents dappInfo vm
  in BoolValue $ null events || not (checkAssertionEvent events)

checkAssertionEvent :: Events -> Bool
checkAssertionEvent = any (T.isPrefixOf "AssertionFailed(")

checkSelfDestructedTarget :: Addr -> DappInfo -> VM -> TestValue
checkSelfDestructedTarget addr _ vm =
  let selfdestructs' = vm ^. (tx . substate . selfdestructs)
  in BoolValue $ addr `notElem` selfdestructs'

checkAnySelfDestructed :: DappInfo -> VM -> TestValue
checkAnySelfDestructed _ vm =
  let sd = vm ^. (tx . substate . selfdestructs)
  in BoolValue $ null sd

checkPanicEvent :: T.Text -> Events -> Bool
checkPanicEvent n = any (T.isPrefixOf ("Panic(" <> n <> ")"))

checkOverflowTest :: DappInfo -> VM -> TestValue
checkOverflowTest dappInfo vm = 
  let es = extractEvents dappInfo vm
  in BoolValue $ null es || not (checkPanicEvent "17" es)
