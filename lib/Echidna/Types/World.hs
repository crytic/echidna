{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.World where

import Control.Lens.TH (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import EVM.Types (Addr)
import GHC.Word (Word32)
import Echidna.Types.Signature (SignatureMap)

-- | The world is made our of humans with an address, and a way to map contract
-- bytecodes to an ABI

data World = World { _senders          :: NonEmpty Addr
                   , _highSignatureMap :: SignatureMap
                   , _lowSignatureMap  :: Maybe SignatureMap
                   , _payableSigs      :: [Word32]
                   }
makeLenses ''World
