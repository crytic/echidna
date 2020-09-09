module Echidna.Types.Buffer where

import Data.ByteString (ByteString)
import EVM.Symbolic (Buffer(..), maybeLitBytes)

viewBuffer :: Buffer -> Maybe ByteString
viewBuffer (ConcreteBuffer b) = Just b
viewBuffer (SymbolicBuffer b) = maybeLitBytes b
