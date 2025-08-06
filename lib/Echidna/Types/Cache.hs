module Echidna.Types.Cache where

import Data.Map (Map)
import Data.Text (Text)
import EVM.Types (W256, Addr, Contract)

type ContractCache = Map Addr (Maybe Contract)
type SlotCache = Map Addr (Map W256 (Maybe W256))
type ContractNameCache = Map W256 Text