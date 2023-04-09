module Echidna.Pretty where

import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Char8 qualified as BSC8
import Data.List (intercalate)
import Data.Text (unpack)

import Echidna.ABI (ppAbiValue)
import Echidna.Types.Signature (SolCall)
import Echidna.Types.Tx (TxCall(..))

-- | Pretty-print some 'AbiCall'.
ppSolCall :: SolCall -> String
ppSolCall (t, vs) =
  (if t == "" then unpack "*fallback*" else unpack t)
  ++ "(" ++ intercalate "," (ppAbiValue <$> vs) ++ ")"

-- | Pretty-print some 'TxCall'
ppTxCall :: TxCall -> String
ppTxCall (SolCreate _)    = "<CREATE>"
ppTxCall (SolCall x)      = ppSolCall x
ppTxCall NoCall           = "*wait*"
ppTxCall (SolCalldata x)  = BSC8.unpack $ "0x" <> BS16.encode x
