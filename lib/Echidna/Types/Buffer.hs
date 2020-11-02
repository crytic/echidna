module Echidna.Types.Buffer where

import Data.ByteString (ByteString)
import EVM.Types (Buffer(..))
import EVM.Symbolic (maybeLitBytes)

viewBuffer :: Buffer -> Maybe ByteString
viewBuffer (ConcreteBuffer b) = Just b
viewBuffer (SymbolicBuffer b) = maybeLitBytes b
