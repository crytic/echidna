module Echidna.Types.Test where

import Prelude hiding (Word)

import Data.Text (Text)
import EVM (VMResult(..), VM)
import EVM.ABI (AbiValue(..), encodeAbiValue)
import EVM.Types (Addr)

import Echidna.Exec
import Echidna.Types.Buffer (viewBuffer)

-- | Configuration for evaluating Echidna tests.
data TestConf = TestConf { classifier :: Text -> VM -> Bool
                           -- ^ Given a VM state and test name, check if a test just passed (typically
                           -- examining '_result'.)
                         , testSender :: Addr -> Addr
                           -- ^ Given the address of a test, return the address to send test evaluation
                           -- transactions from.
                         }

-- | Possible responses to a call to an Echidna test: @true@, @false@, @REVERT@, and ???.
data CallRes = ResFalse | ResTrue | ResRevert | ResOther
  deriving (Eq, Show)

prettyRes :: CallRes -> String
prettyRes ResTrue   = "returned true"
prettyRes ResFalse  = "returned false"
prettyRes ResRevert = "reverted"
prettyRes ResOther  = "failed with another EVM error (..)"

-- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess b) | viewBuffer b == Just (encodeAbiValue (AbiBool True))  = ResTrue
                          | viewBuffer b == Just (encodeAbiValue (AbiBool False)) = ResFalse
                          | otherwise                                             = ResOther
classifyRes Reversion = ResRevert
classifyRes _         = ResOther
