module Echidna.Types.World where

import Data.Set (Set)

import EVM.Types (Addr, FunctionSelector)

import Echidna.Events (EventMap)
import Echidna.Types.Signature (SignatureMap)

-- | The world is composed by:
--    * A list of "human" addresses
--    * A high-priority map of signatures from every contract
--    * A low-priority map of signatures from every contract
--    * A list of function hashes from payable functions
data World = World
  { senders          :: Set Addr
  , highSignatureMap :: SignatureMap
  , lowSignatureMap  :: Maybe SignatureMap
  , payableSigs      :: [FunctionSelector]
  , assertSigs       :: [FunctionSelector]
  , eventMap         :: EventMap
  }
  deriving Show
