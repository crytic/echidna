{-# LANGUAGE GADTs #-}

module Echidna.Test where

import Prelude hiding (Word)

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.ST (RealWorld)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T

import EVM.ABI (AbiValue(..), AbiType(..), encodeAbiValue, decodeAbiValue)
import EVM.Dapp (DappInfo)
import EVM.Types hiding (Env)

import Echidna.ABI
import Echidna.Events (Events, extractEvents)
import Echidna.Exec
import Echidna.Symbolic (forceBuf)
import Echidna.Types.Config
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Test
import Echidna.Types.Tx (Tx, TxConf(..), basicTx, TxResult(..), getResult)

--- | Possible responses to a call to an Echidna test: @true@, @false@, @REVERT@, and ???.
data CallRes = ResFalse | ResTrue | ResRevert | ResOther
  deriving (Eq, Show)

--- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult Concrete s -> CallRes
classifyRes (VMSuccess b)
  | forceBuf b == encodeAbiValue (AbiBool True)  = ResTrue
  | forceBuf b == encodeAbiValue (AbiBool False) = ResFalse
  | otherwise                                    = ResOther
classifyRes Reversion = ResRevert
classifyRes _         = ResOther

getResultFromVM :: VM Concrete s -> TxResult
getResultFromVM vm =
  case vm.result of
    Just r -> getResult r
    Nothing -> error "getResultFromVM failed"

createTest :: TestType -> EchidnaTest
createTest m = EchidnaTest Open m v [] Stop Nothing Nothing
  where v = case m of
              PropertyTest _ _     -> BoolValue True
              OptimizationTest _ _ -> IntValue minBound
              _                    -> NoValue

validateTestModeError :: String
validateTestModeError =
  "Invalid test mode (should be property, assertion, dapptest, optimization, overflow or exploration)"

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

createTests
  :: TestMode
  -> Bool
  -> [Text]
  -> Addr
  -> [SolSignature]
  -> [EchidnaTest]
createTests m td ts r ss = case m of
  "exploration" ->
    [createTest Exploration]
  "overflow" ->
    [createTest (CallTest "Integer (over/under)flow" checkOverflowTest)]
  "property" ->
    map (\t -> createTest (PropertyTest t r)) ts
  "optimization" ->
    map (\t -> createTest (OptimizationTest t r)) ts
  "assertion" ->
    map (\s -> createTest (AssertionTest False s r))
        (filter (/= fallback) ss) ++ [createTest (CallTest "AssertionFailed(..)" checkAssertionTest)]
  "dapptest" ->
    map (\s -> createTest (AssertionTest True s r))
        (filter (\(n, xs) -> T.isPrefixOf "invariant_" n || not (null xs)) ss)
  _ -> error validateTestModeError
  ++ (if td then [sdt, sdat] else [])
  where
  sdt = createTest (CallTest "Target contract is not self-destructed" $ checkSelfDestructedTarget r)
  sdat = createTest (CallTest "No contract can be self-destructed" checkAnySelfDestructed)

  {-
updateOpenTest
  :: EchidnaTest
  -> [Tx]
  -> (TestValue, VM Concrete RealWorld, TxResult)
  -> EchidnaTest
updateOpenTest test txs (BoolValue False, vm, r) =
  test { Test.state = Large 0, reproducer = txs, vm = Just vm, result = r }
updateOpenTest test _   (BoolValue True, _, _) =
  test
updateOpenTest test txs (IntValue v', vm, r) =
  if v' > v then
    test { reproducer = txs
         , value = IntValue v'
         , vm = Just vm
         , result = r }
  else
    test
  where
    v = case test.value of
          IntValue x -> x
          _          -> error "Invalid type of value for optimization"
updateOpenTest _ _ _ = error "Invalid type of test"
-}

-- | Given a 'SolTest', evaluate it and see if it currently passes.
checkETest
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => EchidnaTest
  -> VM Concrete RealWorld
  -> m (TestValue, VM Concrete RealWorld)
checkETest test vm = case test.testType of
  Exploration -> pure (BoolValue True, vm) -- These values are never used
  PropertyTest n a -> checkProperty vm n a
  OptimizationTest n a -> checkOptimization vm n a
  AssertionTest dt n a -> if dt then checkDapptestAssertion vm n a
                                else checkStatefulAssertion vm n a
  CallTest _ f -> checkCall vm f

-- | Given a property test, evaluate it and see if it currently passes.
checkProperty
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> Text
  -> Addr
  -> m (TestValue, VM Concrete RealWorld)
checkProperty vm f a = do
  case vm.result of
    Just (VMSuccess _) -> do
      TestConf{classifier, testSender} <- asks (.cfg.testConf)
      vm' <- runTx vm f testSender a
      pure (BoolValue (classifier f vm'), vm')
    _ -> pure (BoolValue True, vm) -- These values are never used

runTx
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> Text
  -> (Addr -> Addr)
  -> Addr
  -> m (VM Concrete RealWorld)
runTx vm f s a = do
  -- Our test is a regular user-defined test, we exec it and check the result
  g <- asks (.cfg.txConf.propGas)
  (_, vm') <- execTx vm $ basicTx f [] (s a) a g (0, 0)
  pure vm'

--- | Extract a test value from an execution.
getIntFromResult :: Maybe (VMResult Concrete RealWorld) -> TestValue
getIntFromResult (Just (VMSuccess b)) =
  let bs = forceBuf b
  in case decodeAbiValue (AbiIntType 256) $ LBS.fromStrict bs of
    AbiInt 256 n -> IntValue n
    _ -> error ("invalid decode of int256: " ++ show bs)
getIntFromResult _ = IntValue minBound

-- | Given a property test, evaluate it and see if it currently passes.
checkOptimization
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> Text
  -> Addr
  -> m (TestValue, VM Concrete RealWorld)
checkOptimization vm f a = do
  TestConf _ s <- asks (.cfg.testConf)
  vm' <- runTx vm f s a
  pure (getIntFromResult vm'.result, vm')

checkStatefulAssertion
  :: (MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> SolSignature
  -> Addr
  -> m (TestValue, VM Concrete RealWorld)
checkStatefulAssertion vm sig addr = do
  dappInfo <- asks (.dapp)
  let
    -- Whether the last transaction called the function `sig`.
    isCorrectFn =
      BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty))
                    (forceBuf vm.state.calldata)
    -- Whether the last transaction executed a function on the contract `addr`.
    isCorrectAddr = LitAddr addr == vm.state.codeContract
    isCorrectTarget = isCorrectFn && isCorrectAddr
    -- Whether the last transaction executed opcode 0xfe, meaning an assertion failure.
    isAssertionFailure = case vm.result of
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

checkDapptestAssertion
  :: (MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> SolSignature
  -> Addr
  -> m (TestValue, VM Concrete RealWorld)
checkDapptestAssertion vm sig addr = do
  let
    -- Whether the last transaction has any value
    hasValue = vm.state.callvalue /= Lit 0
    -- Whether the last transaction called the function `sig`.
    isCorrectFn =
      BS.isPrefixOf (BS.take 4 (abiCalldata (encodeSig sig) mempty))
                    (forceBuf vm.state.calldata)
    isAssertionFailure = case vm.result of
      Just (VMFailure (Revert (ConcreteBuf bs))) ->
        not $ BS.isSuffixOf assumeMagicReturnCode bs
      Just (VMFailure _) -> True
      _ -> False
    isCorrectAddr = LitAddr addr == vm.state.codeContract
    isCorrectTarget = isCorrectFn && isCorrectAddr
    isFailure = not hasValue && (isCorrectTarget && isAssertionFailure)
  pure (BoolValue (not isFailure), vm)

checkCall
  :: (MonadReader Env m, MonadThrow m)
  => VM Concrete RealWorld
  -> (DappInfo -> VM Concrete RealWorld -> TestValue)
  -> m (TestValue, VM Concrete RealWorld)
checkCall vm f = do
  dappInfo <- asks (.dapp)
  pure (f dappInfo vm, vm)

checkAssertionTest :: DappInfo -> VM Concrete RealWorld -> TestValue
checkAssertionTest dappInfo vm =
  let events = extractEvents False dappInfo vm
  in BoolValue $ null events || not (checkAssertionEvent events)

checkAssertionEvent :: Events -> Bool
checkAssertionEvent = any (T.isPrefixOf "AssertionFailed(")

checkSelfDestructedTarget :: Addr -> DappInfo -> VM Concrete RealWorld -> TestValue
checkSelfDestructedTarget addr _ vm =
  let selfdestructs' = vm.tx.substate.selfdestructs
  in BoolValue $ LitAddr addr `notElem` selfdestructs'

checkAnySelfDestructed :: DappInfo -> VM Concrete RealWorld -> TestValue
checkAnySelfDestructed _ vm =
  BoolValue $ null vm.tx.substate.selfdestructs

checkPanicEvent :: T.Text -> Events -> Bool
checkPanicEvent n = any (T.isPrefixOf ("Panic(" <> n <> ")"))

checkOverflowTest :: DappInfo -> VM Concrete RealWorld-> TestValue
checkOverflowTest dappInfo vm =
  let es = extractEvents False dappInfo vm
  in BoolValue $ null es || not (checkPanicEvent "17" es)

-- | Reproduce a test saving VM snapshot after every transaction
reproduceTest
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => VM Concrete RealWorld -- ^ Initial VM
  -> EchidnaTest
  -> m ([(Tx, VM Concrete RealWorld)], VM Concrete RealWorld)
reproduceTest vm0 test = do
  let txs = test.reproducer
  (results, vm) <- go vm0 [] txs
  (_, vm') <- checkETest test vm
  pure (results, vm')
  where
    go vm executedSoFar toExecute =
      case toExecute of
        [] -> pure ([], vm)
        tx:remainingTxs -> do
          (_, vm') <- execTx vm tx
          (remaining, _) <- go vm' (tx:executedSoFar) remainingTxs
          pure ((tx, vm') : remaining, vm')
