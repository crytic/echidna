{-# LANGUAGE GADTs #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Strict (MonadState(get, put), gets, MonadIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T

import EVM hiding (Env)
import EVM.ABI (AbiValue(..), AbiType(..), encodeAbiValue, decodeAbiValue)
import EVM.Dapp (DappInfo)
import EVM.Types (Addr, Expr (ConcreteBuf, Lit))

import Echidna.ABI
import Echidna.Events (Events, extractEvents)
import Echidna.Exec
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Config
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Test
import Echidna.Types.Tx (Tx, TxConf(..), basicTx, TxResult(..), getResult)

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
  case vm._result of
    Just r -> getResult r
    Nothing -> error "getResultFromVM failed"

createTest :: TestType -> EchidnaTest
createTest m =  EchidnaTest (Open (-1)) m v [] Stop []
                where v = case m of
                           PropertyTest _ _     -> BoolValue True
                           OptimizationTest _ _ -> IntValue minBound
                           _                    -> NoValue

validateTestModeError :: String
validateTestModeError = "Invalid test mode (should be property, assertion, dapptest, optimization, overflow or exploration)"

validateTestMode :: String -> TestMode
validateTestMode s = case s of
  "property"     -> s
  "assertion"    -> s
  "dapptest"     -> s
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

isDapptestMode :: TestMode -> Bool
isDapptestMode "dapptest"  = True
isDapptestMode _           = False

createTests :: TestMode -> Bool -> [Text] -> Addr -> [SolSignature] -> [EchidnaTest]
createTests m td ts r ss = case m of
  "exploration"  -> [createTest Exploration]
  "overflow"     -> [createTest (CallTest "Integer (over/under)flow" checkOverflowTest)]
  "property"     -> map (\t -> createTest (PropertyTest t r)) ts
  "optimization" -> map (\t -> createTest (OptimizationTest t r)) ts
  "assertion"    -> map (\s -> createTest (AssertionTest False s r)) (filter (/= fallback) ss) ++ [createTest (CallTest "AssertionFailed(..)" checkAssertionTest)]
  "dapptest"     -> map (\s -> createTest (AssertionTest True s r)) (filter (\(_, xs) -> not $ null xs) ss)
  _              -> error validateTestModeError

 ++ (if td then [sdt, sdat] else [])
   where sdt  = createTest (CallTest "Target contract is not self-destructed" $ checkSelfDestructedTarget r)
         sdat = createTest (CallTest "No contract can be self-destructed" checkAnySelfDestructed)

updateOpenTest :: EchidnaTest -> [Tx] -> Int -> (TestValue, Events, TxResult) -> EchidnaTest
updateOpenTest test txs _ (BoolValue False,es,r) =
  test { testState = Large (-1), testReproducer = txs, testEvents = es, testResult = r }
updateOpenTest test _   i (BoolValue True,_,_)   =
  test { testState = Open (i + 1) }
updateOpenTest test txs i (IntValue v',es,r) =
  if v' > v then
    test { testState = Open (i + 1)
         , testReproducer = txs
         , testValue = IntValue v'
         , testEvents = es
         , testResult = r }
  else
    test { testState = Open (i + 1) }
  where v = case test.testValue of
              IntValue x -> x
              _          -> error "Invalid type of value for optimization"
updateOpenTest _ _ _ _ = error "Invalid type of test"

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest :: (MonadIO m, MonadReader Env m, MonadState VM m, MonadThrow m)
           => EchidnaTest -> m (TestValue, VM)
checkETest test = case test.testType of
  Exploration -> (BoolValue True,) <$> get -- These values are never used
  PropertyTest n a -> checkProperty n a
  OptimizationTest n a -> checkOptimization n a
  AssertionTest dt n a -> if dt then checkDapptestAssertion n a
                                else checkStatefullAssertion n a
  CallTest _ f -> checkCall f

-- | Given a property test, evaluate it and see if it currently passes.
checkProperty :: (MonadIO m, MonadReader Env m, MonadState VM m, MonadThrow m)
              => Text -> Addr -> m (TestValue, VM)
checkProperty f a = do
  vm <- get
  case vm._result of
    Just (VMSuccess _) -> do
      TestConf{classifier, testSender} <- asks (.cfg.testConf)
      (_, vm') <- runTx f testSender a
      b <- gets $ classifier f
      put vm -- restore EVM state
      pure (BoolValue b, vm')
    _ -> pure (BoolValue True, vm) -- These values are never used

runTx :: (MonadIO m, MonadReader Env m, MonadState VM m, MonadThrow m)
      => Text -> (Addr -> Addr) -> Addr -> m (VM, VM)
runTx f s a = do
  vm <- get -- save EVM state
  -- Our test is a regular user-defined test, we exec it and check the result
  g <- asks (.cfg.txConf.propGas)
  _  <- execTx $ basicTx f [] (s a) a g (0, 0)
  vm' <- get
  return (vm, vm')

--- | Extract a test value from an execution.
getIntFromResult :: Maybe VMResult -> TestValue
getIntFromResult (Just (VMSuccess b)) =
  case viewBuffer b of
    Nothing -> error "invalid decode of buffer"
    Just bs -> case decodeAbiValue (AbiIntType 256) $ LBS.fromStrict bs of
      AbiInt 256 n -> IntValue n
      _ -> error ("invalid decode of int256: " ++ show bs)
getIntFromResult _ = IntValue minBound

-- | Given a property test, evaluate it and see if it currently passes.
checkOptimization :: (MonadIO m, MonadReader Env m, MonadState VM m, MonadThrow m)
                  => Text -> Addr -> m (TestValue, VM)
checkOptimization f a = do
  TestConf _ s <- asks (.cfg.testConf)
  (vm, vm') <- runTx f s a
  put vm -- restore EVM state
  pure (getIntFromResult (vm'._result), vm')

checkStatefullAssertion :: (MonadReader Env m, MonadState VM m, MonadThrow m)
                        => SolSignature -> Addr -> m (TestValue, VM)
checkStatefullAssertion sig addr = do
  dappInfo <- asks (.dapp)
  vm <- get
      -- Whether the last transaction called the function `sig`.
  let isCorrectFn = case viewBuffer vm._state._calldata of
        Just cd -> BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) cd
        Nothing -> False
      -- Whether the last transaction executed a function on the contract `addr`.
      isCorrectAddr = addr == vm._state._codeContract
      isCorrectTarget = isCorrectFn && isCorrectAddr
      -- Whether the last transaction executed opcode 0xfe, meaning an assertion failure.
      isAssertionFailure = case vm._result of
        Just (VMFailure (UnrecognizedOpcode 0xfe)) -> True
        _ -> False
      -- Test always passes if it doesn't target the last executed contract and function.
      -- Otherwise it passes if it doesn't cause an assertion failure.
      events = extractEvents False dappInfo vm
      eventFailure = not (null events) && (checkAssertionEvent events || checkPanicEvent "1" events)
      isFailure = isCorrectTarget && (eventFailure || isAssertionFailure)
  pure (BoolValue (not isFailure), vm)

assumeMagicReturnCode :: BS.ByteString
assumeMagicReturnCode = "FOUNDRY::ASSUME\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"

checkDapptestAssertion :: (MonadReader Env m, MonadState VM m, MonadThrow m)
                       => SolSignature -> Addr -> m (TestValue, VM)
checkDapptestAssertion sig addr = do
  vm <- get
  -- Whether the last transaction has any value
  let hasValue = vm._state._callvalue /= Lit 0
      -- Whether the last transaction called the function `sig`.
  let isCorrectFn = case viewBuffer $ vm._state._calldata of
        Just cd -> BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty)) cd
        Nothing -> False
      isAssertionFailure = case vm._result of
        Just (VMFailure (Revert (ConcreteBuf bs))) -> not $ BS.isSuffixOf assumeMagicReturnCode bs
        Just (VMFailure _)           -> True
        _                            -> False
      isCorrectAddr = addr == vm._state._codeContract
      isCorrectTarget = isCorrectFn && isCorrectAddr
      isFailure = not hasValue && (isCorrectTarget && isAssertionFailure)
  pure (BoolValue (not isFailure), vm)

checkCall :: (MonadReader Env m, MonadState VM m, MonadThrow m)
          => (DappInfo -> VM -> TestValue) -> m (TestValue, VM)
checkCall f = do
  dappInfo <- asks (.dapp)
  vm <- get
  pure (f dappInfo vm, vm)

checkAssertionTest :: DappInfo -> VM -> TestValue
checkAssertionTest dappInfo vm =
  let events = extractEvents False dappInfo vm
  in BoolValue $ null events || not (checkAssertionEvent events)

checkAssertionEvent :: Events -> Bool
checkAssertionEvent = any (T.isPrefixOf "AssertionFailed(")

checkSelfDestructedTarget :: Addr -> DappInfo -> VM -> TestValue
checkSelfDestructedTarget addr _ vm =
  let selfdestructs' = vm._tx._substate._selfdestructs
  in BoolValue $ addr `notElem` selfdestructs'

checkAnySelfDestructed :: DappInfo -> VM -> TestValue
checkAnySelfDestructed _ vm =
  let sd = vm._tx._substate._selfdestructs
  in BoolValue $ null sd

checkPanicEvent :: T.Text -> Events -> Bool
checkPanicEvent n = any (T.isPrefixOf ("Panic(" <> n <> ")"))

checkOverflowTest :: DappInfo -> VM -> TestValue
checkOverflowTest dappInfo vm =
  let es = extractEvents False dappInfo vm
  in BoolValue $ null es || not (checkPanicEvent "17" es)
