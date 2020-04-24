{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.World where

import Control.Lens.TH (makeLenses)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.HashMap.Strict (HashMap)
import EVM.Types (Addr)

import Echidna.Types.Signature (SolSignature)

-- | The world is made our of humans with an address, and a way to map contract
-- bytecodes to an ABI
data World = World { _senders         :: NonEmpty Addr
                   , _bytecodeMapping :: HashMap ByteString (NonEmpty SolSignature)
                   }
makeLenses ''World
