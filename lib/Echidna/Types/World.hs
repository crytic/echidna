{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.World where

import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty)

import EVM.Types (Addr)

import Echidna.Types.Signature (FunctionHash, SignatureMap)
import Echidna.Events (EventMap)

-- | The world is composed by:
--    * A list of "human" addresses
--    * A high-priority map of signatures from every contract
--    * A low-priority map of signatures from every contract
--    * A list of function hashes from payable functions
data World = World { _senders          :: NonEmpty Addr
                   , _highSignatureMap :: SignatureMap
                   , _lowSignatureMap  :: Maybe SignatureMap
                   , _payableSigs      :: [FunctionHash]
                   , _eventMap         :: EventMap
                   }
makeLenses ''World
