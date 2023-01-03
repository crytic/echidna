{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Echidna.Types.Buffer where

import Data.ByteString (ByteString)
import EVM.Types (Expr(ConcreteBuf), EType(Buf))

viewBuffer :: Expr 'Buf -> Maybe ByteString
viewBuffer (ConcreteBuf b) = Just b
viewBuffer _ = Nothing
