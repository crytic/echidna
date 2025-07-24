{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}

module Echidna.Events where

import Control.Monad.ST (RealWorld)
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (fromStrict)
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, catMaybes, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack, Text)
import Data.Tree (flatten)
import Data.Tree.Zipper (fromForest, TreePos, Empty)
import Data.Vector (fromList)

import EVM (traceForest)
import EVM.ABI (Event(..), Indexed(..), decodeAbiValue, getAbi, AbiType(..), AbiValue(..))
import EVM.Dapp (DappContext(..), DappInfo(..))
import EVM.Expr (maybeLitWordSimp)
import EVM.Format (showValues, showError, contractNamePart)
import EVM.Solidity (SolcContract(..))
import EVM.Types
import Echidna.Types.Tx

import Echidna.Symbolic (forceWord, forceBuf)

type EventMap = Map W256 Event
type Events = [Text]

emptyEvents :: TreePos Empty a
emptyEvents = fromForest []

extractEvents :: Bool -> DappInfo -> VM Concrete s -> Events
extractEvents decodeErrors dappInfo vm =
  let forest = traceForest vm
  in maybeToList (decodeRevert decodeErrors vm)
     ++ concatMap ((catMaybes . flatten) . fmap showTrace) forest
  where
  showTrace trace =
    let ?context = DappContext { info = dappInfo, contracts = vm.env.contracts, labels = vm.labels } in
    let codehash' = fromJust $ maybeLitWordSimp trace.contract.codehash
        maybeContractName = maybeContractNameFromCodeHash dappInfo codehash'
    in case trace.tracedata of
      EventTrace addr bytes (topic:_) ->
        case Map.lookup (forceWord topic) dappInfo.eventMap of
          Just (Event name _ types) ->
            -- TODO this is where indexed types are filtered out
            -- they are filtered out for a reason as they only contain
            -- the topic hash which is printed super verbose by dapptools
            Just $
              name
              <> showValues [t | (_, t, NotIndexed) <- types] bytes
              <> " from: "
              <> maybe mempty (<> "@") maybeContractName
              <> pack (show $ forceWord addr)
          Nothing -> Just $ pack $ show (forceWord topic)
      ErrorTrace e ->
        case e of
          Revert out ->
            Just $
              "error Revert "
              <> showError out
              <> maybe "" (": " <>) (maybeHumanPanic (forceBuf out))
              <> maybe "" (" from: " <>) maybeContractName
          _ ->
            Just $ "error " <> pack (show e)
      _ -> Nothing

  maybeHumanPanic bs =
    case BS.splitAt 4 bs of
      ("\x4e\x48\x7b\x71", d) ->
        Just $ humanPanic $ decodePanic d
      _ -> Nothing

-- Extract all non-indexed `uint256` values from events emitted in the given VM results.
-- Returns a map from `AbiType` to the set of unique `AbiValue`s found in those events.

extractEventValues
  :: DappInfo
  -> [(Tx, VMResult Concrete RealWorld)]
  -> Map AbiType (Set AbiValue)
extractEventValues dappInfo txResults =
  let
    -- for each transaction result, get its logs
    allLogs :: [Expr Log]
    allLogs = concatMap (\case
      (_, VMSuccess _ dataLogs _) -> dataLogs
      _                           -> []
      ) txResults

    -- decode a single Expr Log into zero or more (ty, val) pairs
    goLog = \case
      LogEntry _addr dataBuf (sigHash : _) ->
        case Map.lookup (forceWord sigHash) dappInfo.eventMap of
          Just (Event _name _sig params) ->
            -- decode every _non‑indexed_ param as Uint256
            [ (UintType, UintValue v)
            | (_, _, idxKind) <- params
            , idxKind == NotIndexed
            , Right (_, _, v) <- [ runGetOrFail (getAbi UintType)
                                              (LBS.fromStrict dataBuf) ]
            ]
          _ -> []
      _ -> []

    pairs = concatMap goLog allLogs
  in
    foldl' (\m (ty,v) -> Map.insertWith Set.union ty (Set.singleton v) m)
           Map.empty
           pairs

maybeContractNameFromCodeHash :: DappInfo -> W256 -> Maybe Text
maybeContractNameFromCodeHash info codeHash = contractToName <$> maybeContract
  where maybeContract = snd <$> Map.lookup codeHash info.solcByHash
        contractToName c = contractNamePart c.contractName

decodeRevert :: Bool -> VM Concrete s -> Maybe Text
decodeRevert decodeErrors vm =
  case vm.result of
    Just (VMFailure (Revert (ConcreteBuf bs))) -> decodeRevertMsg decodeErrors bs
    _ -> Nothing

decodeRevertMsg :: Bool -> ByteString -> Maybe Text
decodeRevertMsg decodeErrors bs =
  case BS.splitAt 4 bs of
    ("\x08\xc3\x79\xa0", d) | decodeErrors ->
      Just $ "Error" <> (pack . show $ decodeAbiValue (AbiTupleType (fromList [AbiStringType])) (fromStrict d))
    ("\x4e\x48\x7b\x71", d) ->
      Just $ "Panic(" <> (pack . show $ decodePanic d) <> "): " <> humanPanic (decodePanic d)
    _ -> Nothing

decodePanic :: ByteString -> AbiValue
decodePanic v = decodeAbiValue (AbiUIntType 256) (fromStrict v)

humanPanic :: AbiValue -> Text
humanPanic (AbiUInt _ code) =
  case code of
    0x01 -> "Using assert"
    0x11 -> "SafeMath over-/under-flows"
    0x12 -> "Divide by 0"
    0x21 -> "Conversion into non-existent enum type"
    0x22 -> "Incorrectly encoded storage byte array"
    0x31 -> "pop() on an empty array"
    0x32 -> "Index out of bounds exception"
    0x41 -> "Allocating too much memory or creating a too large array"
    0x51 -> "Calling a zero-initialized variable of internal function type"
    _ -> "Unknown panic error code"
humanPanic _ =
  error "Shouldn't happen, improve types to make this branch go away"
