module Echidna.Types.Test where

import Data.Aeson (ToJSON(..), object)
import Data.DoubleWord (Int256)
import Data.Maybe (maybeToList)
import Data.Text (Text)

import EVM (VM)
import EVM.Types (Addr)
import EVM.Dapp (DappInfo)

import Echidna.Events (Events)
import Echidna.Types
import Echidna.Types.Tx (Tx, TxResult)
import Echidna.Types.Signature (SolSignature)

-- | Test mode is parsed from a string
type TestMode = String

-- | Configuration for the creation of Echidna tests.
data TestConf = TestConf { classifier :: Text -> VM -> Bool
                           -- ^ Given a VM state and test name, check if a test just passed (typically
                           -- examining '_result'.)
                         , testSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         }

type TestAttempts = Int

-- | State of a particular Echidna test. N.B.: \"Solved\" means a falsifying call sequence was found.
data TestState = Open Int        -- ^ Maybe solvable, tracking attempts already made
               | Large Int       -- ^ Solved, maybe shrinable, tracking shrinks tried
               | Passed          -- ^ Presumed unsolvable
               | Solved          -- ^ Solved with no need for shrinking
               | Failed ExecException -- ^ Broke the execution environment
                 deriving Show

data TestValue = BoolValue Bool
               | IntValue Int256
               | NoValue
                 deriving (Eq)

instance Show TestValue where
  show (BoolValue x) = show x
  show (IntValue x)  = show x
  show NoValue       = ""

data TestType = PropertyTest Text Addr
              | OptimizationTest Text Addr
              | AssertionTest Bool SolSignature Addr
              | CallTest Text (DappInfo -> VM -> TestValue)
              | Exploration

instance Eq TestType where
  (PropertyTest t a)     == (PropertyTest t' a')     = t == t' && a == a'
  (AssertionTest b s a)  == (AssertionTest b' s' a') = b == b' && s == s' && a == a'
  (OptimizationTest s a) == (OptimizationTest s' a') = s == s' && a == a'
  (CallTest t _)         == (CallTest t' _)          = t == t'
  Exploration            == Exploration              = True
  _                      == _                        = False


instance Eq TestState where
  (Open i)  == (Open j)    = i == j
  (Large i) == (Large j)   = i == j
  Passed    == Passed      = True
  Solved    == Solved      = True
  _         == _           = False

-- | An Echidna test is represented with the following data record
data EchidnaTest = EchidnaTest {
                                 testState      :: TestState
                               , testType       :: TestType
                               , testValue      :: TestValue
                               , testReproducer :: [Tx]
                               , testResult     :: TxResult
                               , testEvents     :: Events
                               } deriving Eq

isOptimizationTest :: TestType -> Bool
isOptimizationTest (OptimizationTest _ _) = True
isOptimizationTest _                      = False

isOpen :: EchidnaTest -> Bool
isOpen t = case t.testState of
            Open _ -> True
            _      -> False

didFailed :: EchidnaTest -> Bool
didFailed t = case t.testState of
              Large _ -> True
              Solved  -> True
              _       -> False

isPassed :: EchidnaTest -> Bool
isPassed t = case t.testState of
              Passed -> True
              _      -> False


instance ToJSON TestState where
  toJSON s = object $ ("passed", toJSON passed) : maybeToList desc where
    (passed, desc) = case s of Open _    -> (True, Nothing)
                               Passed    -> (True, Nothing)
                               Large _   -> (False, Nothing)
                               Solved    -> (False, Nothing)
                               Failed  e -> (False, Just ("exception", toJSON $ show e))
