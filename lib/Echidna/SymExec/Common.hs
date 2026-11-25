{-# LANGUAGE GADTs #-}

module Echidna.SymExec.Common where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DoubleWord (Word256)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

import EVM.ABI (selector, encodeAbiValue, AbiValue(..))
import EVM.Fetch qualified as Fetch
import EVM.Solidity (Method(..))
import EVM.SymExec (Postcondition)
import EVM.Types (Addr, EType(..), EvmError(..), Expr(..), Prop(..), W256)

import Echidna.Types.Tx (Tx)

-- | Enable hevm's arithmetic abstraction for symbolic exploration and verification.
symExecAbstractArith :: Bool
symExecAbstractArith = True

panicMsg :: Word256 -> ByteString
panicMsg err = selector "Panic(uint256)" <> encodeAbiValue (AbiUInt 256 err)

-- | Postcondition for symbolic execution verification.
-- In foundry mode, all reverts are failures except vm.assume failures.
-- In assertion mode, only assertion failures (0xfe opcode, Error(string)
-- "assertion failed", or Panic codes) are detected.
checkAssertions :: [Word256] -> Bool -> Postcondition
checkAssertions errs isFoundry _ vmres
  | isFoundry = case vmres of
      -- vm.assume failures should not be treated as test failures
      Failure _ _ AssumeCheatFailed -> PBool True
      -- All other failures are test failures in foundry mode
      Failure {} -> PBool False
      _ -> PBool True
  | otherwise = case vmres of
      -- Solidity assert() opcode (0xfe)
      Failure _ _ (UnrecognizedOpcode 0xfe) -> PBool False
      -- Concrete revert: check for "assertion failed" message or panic code
      Failure _ _ (Revert (ConcreteBuf msg)) ->
        -- NOTE: assertTrue/assertFalse does not have the double colon after "assertion failed"
        let assertFail = selector "Error(string)" `BS.isPrefixOf` msg
              && "assertion failed" `BS.isPrefixOf` BS.drop txtOffset msg
        in PBool $ not (assertFail || msg `elem` fmap panicMsg errs)
      -- Symbolic revert: check symbolically against panic messages
      -- TODO: also check for Error(string) "assertion failed" in partially-symbolic
      -- buffers, similar to hevm's symbolicFail in EVM.UnitTest
      Failure _ _ (Revert b) ->
        foldl' PAnd (PBool True) (fmap (PNeg . PEq b . ConcreteBuf . panicMsg) errs)
      _ -> PBool True
  where
    txtOffset = 4 + 32 + 32 -- selector + offset + length

type PartialsLogs = [T.Text]

data TxOrError = TxFromResult [Tx] | SMTErrorFromResult String
  deriving (Show)

extractTxs :: [TxOrError] -> [[Tx]]
extractTxs = mapMaybe (\case
  TxFromResult tx -> Just tx
  _ -> Nothing)

extractErrors :: [TxOrError] -> [String]
extractErrors = mapMaybe (\case
  SMTErrorFromResult err -> Just err
  _ -> Nothing)

suitableForSymExec :: Method -> Bool
suitableForSymExec m =
  -- the method must take arguments (otherwise there is nothing to solve for)
  -- and must not opt out via a `_no_symexec` name. Dynamic ABI types
  -- (bytes/string/dynamic arrays) are now supported: hevm concretizes them to a
  -- bounded length (see maxDynSize), so they no longer disqualify a method.
  not (null m.inputs)
  && not (T.isInfixOf "_no_symexec" m.name)

senderConstraints :: Expr EAddr -> Set Addr -> [Prop]
senderConstraints caller as = [foldr (\a b -> POr b (PEq caller (LitAddr a))) (PBool False) $ Set.toList as]

rpcFetcher :: Functor f =>
  f a -> Maybe W256 -> f (Fetch.BlockNumber, a)

rpcFetcher rpc block = (,) block' <$> rpc
  where block' = maybe Fetch.Latest Fetch.BlockNumber block
