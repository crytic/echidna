module Echidna.Types.World where

import EVM.Types (Addr)

import Echidna.Types.Signature (FunctionHash, SignatureMap)
import Echidna.Events (EventMap)
import Data.Set (Set)

-- | The world is composed by:
--    * A list of "human" addresses
--    * A high-priority map of signatures from every contract
--    * A low-priority map of signatures from every contract
--    * A list of function hashes from payable functions
data World = World { senders          :: Set Addr
                   , highSignatureMap :: SignatureMap
                   , lowSignatureMap  :: Maybe SignatureMap
                   , payableSigs      :: [FunctionHash]
                   , eventMap         :: EventMap
                   }
