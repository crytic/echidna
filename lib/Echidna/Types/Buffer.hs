{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Echidna.Types.Buffer where

import Data.ByteString (ByteString)
import EVM.Types (Expr(ConcreteBuf, Lit), EType(Buf), EType(EWord), W256)

viewBuffer :: Expr 'Buf -> Maybe ByteString
viewBuffer (ConcreteBuf b) = Just b
viewBuffer _ = Nothing

forceLit :: Expr 'EWord -> W256
forceLit x = case x of
  Lit x' -> x'
  _ -> error "expected Lit"
