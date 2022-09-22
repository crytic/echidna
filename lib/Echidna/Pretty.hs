{-# LANGUAGE OverloadedStrings #-}

module Echidna.Pretty where

import Data.List (intercalate)
import Data.Text (unpack)

import Echidna.Types.Signature (SolCall)
import Echidna.Types.Tx (TxCall(..))

import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BSC8

-- | Pretty-print some 'AbiCall'.
ppSolCall :: SolCall -> String
ppSolCall (t, vs) =
  (if t == "" then "*fallback*" else unpack t)
  ++ "(" ++ intercalate "," (show <$> vs) ++ ")"

-- | Pretty-print some 'TxCall'
ppTxCall :: TxCall -> String
ppTxCall (SolCreate _)    = "<CREATE>"
ppTxCall (SolCall x)      = ppSolCall x
ppTxCall NoCall           = "*wait*"
ppTxCall (SolCalldata x)  = BSC8.unpack $ "0x" <> BS16.encode x
