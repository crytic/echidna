{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Test where

import Control.Lens
import Data.Aeson (ToJSON(..), object)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import EVM (VM)
import EVM.Types (Addr)

import Echidna.Exec (ExecException)
import Echidna.Events (Events, EventMap)
import Echidna.Types.Tx (Tx, TxResult)
import Echidna.Types.Signature (SolSignature)

type TestMode = String

-- | Configuration for the creation of Echidna tests.
data TestConf = TestConf { classifier :: Text -> VM -> Bool
                           -- ^ Given a VM state and test name, check if a test just passed (typically
                           -- examining '_result'.)
                         , testSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         }

-- | An Echidna test is either the name of the function to call and the address where its contract is,
-- or a function that could experience an exception
--type SolTest = Either (Text, Addr) SolSignature

-- | State of a particular Echidna test. N.B.: \"Solved\" means a falsifying call sequence was found.
data TestState = Open Int             -- ^ Maybe solvable, tracking attempts already made
               | Large Int [Tx]       -- ^ Solved, maybe shrinable, tracking shrinks tried + best solve
               | Passed               -- ^ Presumed unsolvable
               | Solved [Tx]          -- ^ Solved with no need for shrinking
               | Failed ExecException -- ^ Broke the execution environment
                 deriving Show

data TestType = PropertyTest Text Addr 
              | AssertionTest SolSignature Addr 
              | CallTest Text (EventMap -> VM -> Bool) 
              | MinTest Text (EventMap -> VM -> Int) 
              | Exploration

instance Eq TestType where -- MinTest is missing
  (PropertyTest t a)  == (PropertyTest t' a')  = t == t' && a == a'
  (AssertionTest s a) == (AssertionTest s' a') = s == s' && a == a'
  (CallTest t _)      == (CallTest t' _)       = t == t'
  Exploration         == Exploration           = True
  _                   == _                     = False


instance Eq TestState where
  (Open i)    == (Open j)    = i == j
  (Large i l) == (Large j m) = i == j && l == m
  Passed      == Passed      = True
  (Solved l)  == (Solved m)  = l == m
  _           == _           = False


data EchidnaTest = EchidnaTest { 
                                 _testState :: TestState
                               , _testType :: TestType
                               , _testResult :: TxResult
                               , _testEvents :: Events 
                               } deriving Eq  

makeLenses ''EchidnaTest

instance ToJSON TestState where
  toJSON s = object $ ("passed", toJSON passed) : maybeToList desc where
    (passed, desc) = case s of Open _    -> (True, Nothing)
                               Passed    -> (True, Nothing)
                               Large _ l -> (False, Just ("callseq", toJSON l))
                               Solved  l -> (False, Just ("callseq", toJSON l))
                               Failed  e -> (False, Just ("exception", toJSON $ show e))
