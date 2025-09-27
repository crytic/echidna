module Echidna.Pretty where

import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BSC8
import Data.List (intercalate)
import Data.Map (Map)
import Data.Text (Text, unpack)

import EVM.Types (Addr)

import Echidna.ABI (ppAbiValue)
import Echidna.Types.Signature (SolCall)
import Echidna.Types.Tx (Tx(..), TxCall(..))

-- | Pretty-print some 'AbiCall'.
ppSolCall :: Map Addr Text -> SolCall -> String
ppSolCall labels (t, vs) =
  (if t == "" then unpack "*fallback*" else unpack t)
  ++ "(" ++ intercalate "," (ppAbiValue labels <$> vs) ++ ")"

-- | Pretty-print some 'TxCall'
ppTxCall :: Map Addr Text -> TxCall -> String
ppTxCall _ (SolCreate _)    = "<CREATE>"
ppTxCall labels (SolCall x) = ppSolCall labels x
ppTxCall _ NoCall           = "*wait*"
ppTxCall _ (SolCalldata x)  = BSC8.unpack $ "0x" <> BS16.encode x

-- | Pretty-print some 'Tx'
ppTx :: Map Addr Text -> Tx -> String
ppTx labels tx = ppTxCall labels (tx.call)
