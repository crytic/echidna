{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.Types.Test where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.DoubleWord (Int256)
import Data.Maybe (maybeToList)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)

import EVM.ABI (selector)
import EVM.Dapp (DappInfo)
import EVM.Types (Addr, VM, VMType(Concrete))

import Echidna.ABI (encodeSig)
import Echidna.Types (ExecException)
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Tx (Tx, TxResult)

-- | Test mode is parsed from a string
type TestMode = String

-- | Configuration for the creation of Echidna tests.
data TestConf = TestConf
  { classifier :: Text -> VM Concrete -> Bool
    -- ^ Given a VM state and test name, check if a test just passed (typically
    -- examining '_result'.)
  , testSender :: Addr -> Addr
    -- ^ Given the address of a test, return the address to send test evaluation
    -- transactions from.
  }

-- | State of a particular Echidna test. N.B.: 'Solved' means a falsifying
-- call sequence was found.
data TestState
  = Open
  | Large !Int -- ^ Solved, maybe shrinkable, tracking shrinks tried
  | Passed     -- ^ Presumed unsolvable
  | Unsolvable -- ^ Formally verified as unsolvable
  | Solved     -- ^ Solved with no need for shrinking
  | Failed ExecException -- ^ Broke the execution environment
  deriving Show

data TestValue
  = BoolValue Bool
  | IntValue Int256
  | NoValue
  deriving (Eq, Ord, Generic, ToJSON)

instance Show TestValue where
  show (BoolValue x) = show x
  show (IntValue x)  = show x
  show NoValue       = ""

data TestType
  = PropertyTest Text Addr
  | OptimizationTest Text Addr
  | AssertionTest
      { foundry :: Bool
        -- ^ Foundry-style test?
      , sig :: SolSignature
        -- ^ the function under test
      , addr :: Addr
        -- ^ the contract under test
      , sel :: ByteString
        -- ^ 4-byte selector of 'sig'. Derived; cached here because the check
        -- runs for every open test after every call and hashing the signature
        -- each time dominated it. Build with 'assertionTest'.
      }
  | CallTest Text (DappInfo -> VM Concrete -> TestValue)
  | Exploration

-- | An 'AssertionTest' with its selector filled in.
assertionTest :: Bool -> SolSignature -> Addr -> TestType
assertionTest foundry sig addr =
  AssertionTest { foundry, sig, addr, sel = selector (encodeSig sig) }

instance Eq TestType where
  -- 'sel' is ignored: it is derived from 'sig'.
  PropertyTest t a      == PropertyTest t' a'       = t == t' && a == a'
  AssertionTest b s a _ == AssertionTest b' s' a' _ = b == b' && s == s' && a == a'
  OptimizationTest s a  == OptimizationTest s' a'   = s == s' && a == a'
  CallTest t _          == CallTest t' _            = t == t'
  Exploration           == Exploration              = True
  _                     == _                        = False

instance Show TestType where
  show = \case
    PropertyTest t _     -> show t
    AssertionTest{sig}   -> show sig
    OptimizationTest s _ -> show s
    CallTest t _         -> show t
    Exploration          -> "Exploration"

instance ToJSON TestType where
  toJSON = \case
    PropertyTest name addr ->
      object [ "type" .= ("property_test" :: String), "name" .= name, "addr" .= addr ]
    OptimizationTest name addr ->
      object [ "type" .= ("optimization_test" :: String), "name" .= name, "addr" .= addr ]
    AssertionTest{sig, addr} ->
      object [ "type" .= ("assertion_test" :: String), "signature" .= sig, "addr" .= addr ]
    CallTest name _ ->
      object [ "type" .= ("call_test" :: String), "name" .= name ]
    Exploration ->
      object [ "type" .= ("exploration_test" :: String) ]

instance Eq TestState where
  Open       == Open       = True
  Large i    == Large j    = i == j
  Passed     == Passed     = True
  Solved     == Solved     = True
  Unsolvable == Unsolvable = True
  _          == _          = False

-- | An Echidna test is represented with the following data record
data EchidnaTest = EchidnaTest
  { state      :: TestState
  , testType   :: TestType
  , value      :: TestValue
  , reproducer :: [Tx]
  , result     :: TxResult
  , vm         :: Maybe (VM Concrete)
  -- | Worker which falsified the test will also shrink it.
  , workerId   :: Maybe Int
  } deriving (Show)

instance ToJSON EchidnaTest where
  toJSON EchidnaTest{..} = object
    [ "state" .= state
    , "type" .= testType
    , "value" .= value
    , "reproducer" .= reproducer
    , "result" .= result
    ]

isOptimizationTest :: EchidnaTest -> Bool
isOptimizationTest EchidnaTest{testType = OptimizationTest _ _} = True
isOptimizationTest _ = False

isAssertionTest :: EchidnaTest -> Bool
isAssertionTest EchidnaTest{testType = AssertionTest {}} = True
isAssertionTest _ = False

getAssertionSignature :: EchidnaTest -> String
getAssertionSignature EchidnaTest{testType = AssertionTest{sig}} = unpack $ encodeSig sig
getAssertionSignature _ = error "Not an assertion test"

getAssertionFunctionName :: EchidnaTest -> String
getAssertionFunctionName EchidnaTest{testType = AssertionTest{sig = (name, _)}} = unpack name
getAssertionFunctionName _ = error "Not an assertion test"

isOpen :: EchidnaTest -> Bool
isOpen t = case t.state of
  Open -> True
  _    -> False

-- | Whether a test still needs shrinking. This includes optimization tests
-- that haven't been closed yet (Open) and any test mid-shrink (Large).
needsShrinking :: EchidnaTest -> Bool
needsShrinking t =
  case t.state of
    Large _ -> True
    Open    -> isOptimizationTest t
    _       -> False

didFail :: EchidnaTest -> Bool
didFail t = case t.state of
  Large _ -> True
  Solved  -> True
  _       -> False

-- | Whether a test has conclusively failed, which is what @stopOnFail@ acts on.
-- A falsified test being shrunk ('Large') does not count yet: it only becomes
-- 'Solved' once shrinking is done.
isConclusiveFailure :: EchidnaTest -> Bool
isConclusiveFailure t = case t.state of
  Solved   -> True
  Failed _ -> True
  _        -> False

isPassed :: EchidnaTest -> Bool
isPassed t = case t.state of
  Passed -> True
  _      -> False

isVerified :: EchidnaTest -> Bool
isVerified t = case t.state of
  Unsolvable -> True
  _          -> False

-- | Whether a whole test set should be reported as a success or a failure.
-- This is what decides Echidna's exit code.
--
-- 'Unsolvable' counts as a success: it is the state verification mode sets on
-- a test it has formally proven cannot be falsified.
isSuccessful :: [EchidnaTest] -> Bool
isSuccessful = all (\t -> isOpen t || isPassed t || isVerified t)

instance ToJSON TestState where
  toJSON s =
    object $ ("passed", toJSON passed) : maybeToList desc
    where
    (passed, desc) = case s of
      Open     -> (True, Nothing)
      Passed   -> (True, Nothing)
      Unsolvable -> (True, Nothing)
      Large _  -> (False, Nothing)
      Solved   -> (False, Nothing)
      Failed e -> (False, Just ("exception", toJSON $ show e))
