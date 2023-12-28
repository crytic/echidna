{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Symbolic where

import Data.ByteString (ByteString)
import EVM.Types (Expr(..), EType(..), W256, Addr)

forceBuf :: Expr Buf -> ByteString
forceBuf b = case b of
  ConcreteBuf b' -> b'
  _ -> error $ "expected ConcreteBuf: " <> show b

forceWord :: Expr EWord -> W256
forceWord x = case x of
  Lit x' -> x'
  WAddr x' -> fromIntegral $ forceAddr x'
  _ -> error $ "expected Lit: " <> show x

forceAddr :: Expr EAddr -> Addr
forceAddr x = case x of
  LitAddr x' -> x'
  _ -> error $ "expected LitAddr: " <> show x
