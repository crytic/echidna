module Echidna.Pretty where

import Echidna.Solidity.Types (SolCall)

import Data.Foldable (toList)
import Data.List     (intercalate)
import Data.Text     (unpack)
import EVM.ABI       (AbiValue(..))
import Numeric       (showHex)

-- | Pretty-print some 'AbiValue'.
ppAbiValue :: AbiValue -> String
ppAbiValue (AbiUInt _ n)         = show n
ppAbiValue (AbiInt  _ n)         = show n
ppAbiValue (AbiAddress n)        = showHex n ""
ppAbiValue (AbiBool b)           = if b then "true" else "false"
ppAbiValue (AbiBytes      _ b)   = show b
ppAbiValue (AbiBytesDynamic b)   = show b
ppAbiValue (AbiString       s)   = show s
ppAbiValue (AbiArrayDynamic _ v) =
  "[" ++ intercalate ", " (ppAbiValue <$> toList v) ++ "]"
ppAbiValue (AbiArray      _ _ v) =
  "[" ++ intercalate ", " (ppAbiValue <$> toList v) ++ "]"
ppAbiValue (AbiTuple v) =
  "(" ++ intercalate ", " (ppAbiValue <$> toList v) ++ ")"

-- | Pretty-print some 'SolCall'.
ppSolCall :: SolCall -> String
ppSolCall (t, vs) = (if t == "" then unpack "*fallback*" else unpack t) ++ "(" ++ intercalate "," (ppAbiValue <$> vs) ++ ")"
