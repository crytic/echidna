module Echidna.Types.Cache where

import Data.Map (Map)
import Data.Text (Text)

import EVM.Types (W256)

type ContractNameCache = Map W256 Text
