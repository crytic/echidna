{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Types.Buffer where

import Data.ByteString (ByteString)
import EVM.Types (Expr(ConcreteBuf, Lit), EType(Buf, EWord), W256)

forceBuf :: Expr 'Buf -> ByteString
forceBuf (ConcreteBuf b) = b
forceBuf _ = error "expected ConcreteBuf"

forceLit :: Expr 'EWord -> W256
forceLit x = case x of
  Lit x' -> x'
  _ -> error "expected Lit"
