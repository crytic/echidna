module Echidna.Types.Test where

import Data.Aeson (ToJSON(..), object)
import Data.DoubleWord (Int256)
import Data.Maybe (maybeToList)
import Data.Text (Text)

import EVM.Dapp (DappInfo)
import EVM.Types (Addr, VM)

import Echidna.Events (Events)
import Echidna.Types (ExecException)
import Echidna.Types.Signature (SolSignature)
import Echidna.Types.Tx (Tx, TxResult)

-- | Test mode is parsed from a string
type TestMode = String

-- | Configuration for the creation of Echidna tests.
data TestConf = TestConf
  { classifier :: Text -> VM -> Bool
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
  | Large !Int -- ^ Solved, maybe shrinable, tracking shrinks tried
  | Passed     -- ^ Presumed unsolvable
  | Solved     -- ^ Solved with no need for shrinking
  | Failed ExecException -- ^ Broke the execution environment
  deriving Show

data TestValue
  = BoolValue Bool
  | IntValue Int256
  | NoValue
  deriving (Eq, Ord)

instance Show TestValue where
  show (BoolValue x) = show x
  show (IntValue x)  = show x
  show NoValue       = ""

data TestType
  = PropertyTest Text Addr
  | OptimizationTest Text Addr
  | AssertionTest Bool SolSignature Addr
  | CallTest Text (DappInfo -> VM -> TestValue)
  | Exploration

instance Eq TestType where
  PropertyTest t a     == PropertyTest t' a'     = t == t' && a == a'
  AssertionTest b s a  == AssertionTest b' s' a' = b == b' && s == s' && a == a'
  OptimizationTest s a == OptimizationTest s' a' = s == s' && a == a'
  CallTest t _         == CallTest t' _          = t == t'
  Exploration          == Exploration            = True
  _                    == _                      = False

instance Show TestType where
  show = \case
    PropertyTest t _     -> show t
    AssertionTest _ s _  -> show s
    OptimizationTest s _ -> show s
    CallTest t _         -> show t
    Exploration          -> "Exploration"

instance Eq TestState where
  Open    == Open    = True
  Large i == Large j = i == j
  Passed  == Passed  = True
  Solved  == Solved  = True
  _       == _       = False

-- | An Echidna test is represented with the following data record
data EchidnaTest = EchidnaTest
  { state      :: TestState
  , testType   :: TestType
  , value      :: TestValue
  , reproducer :: [Tx]
  , result     :: TxResult
  , events     :: Events
  } deriving (Eq, Show)

isOptimizationTest :: EchidnaTest -> Bool
isOptimizationTest EchidnaTest{testType = OptimizationTest _ _} = True
isOptimizationTest _ = False

isOpen :: EchidnaTest -> Bool
isOpen t = case t.state of
  Open -> True
  _    -> False

didFail :: EchidnaTest -> Bool
didFail t = case t.state of
  Large _ -> True
  Solved  -> True
  _       -> False

isPassed :: EchidnaTest -> Bool
isPassed t = case t.state of
  Passed -> True
  _      -> False

instance ToJSON TestState where
  toJSON s =
    object $ ("passed", toJSON passed) : maybeToList desc
    where
    (passed, desc) = case s of
      Open     -> (True, Nothing)
      Passed   -> (True, Nothing)
      Large _  -> (False, Nothing)
      Solved   -> (False, Nothing)
      Failed e -> (False, Just ("exception", toJSON $ show e))
