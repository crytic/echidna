{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GADTs #-}

module Echidna.Events where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy (fromStrict)
import Data.Tree (flatten)
import Data.Tree.Zipper (fromForest, TreePos, Empty)
import Data.Text (pack, Text)
import Data.Map qualified as M
import Data.Maybe (listToMaybe, fromJust)
import Data.Vector (fromList)

import EVM
import EVM.ABI (Event(..), Indexed(..), decodeAbiValue, AbiType(AbiUIntType, AbiTupleType, AbiStringType))
import EVM.Dapp (DappContext(..), DappInfo(..))
import EVM.Format (showValues, showError, contractNamePart)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Expr(ConcreteBuf), W256, maybeLitWord)

import Echidna.Types.Buffer (forceLit)
import qualified Data.Map as Map

type EventMap = M.Map W256 Event
type Events = [Text]

emptyEvents :: TreePos Empty a
emptyEvents = fromForest []

maybeContractNameFromCodeHash :: (?context :: DappContext) => EVM.Types.W256 -> Maybe Text
maybeContractNameFromCodeHash codeHash = fmap contractToName maybeContract
  where maybeContract = snd <$> Map.lookup codeHash ?context.info.solcByHash
        contractToName c = contractNamePart c.contractName

extractEvents :: Bool -> DappInfo -> VM -> Events
extractEvents decodeErrors dappInfo vm =
  let eventMap = dappInfo.eventMap
      forest = traceForest vm
      showTrace trace =
        let ?context = DappContext { info = dappInfo, env = vm._env._contracts } in
        let codehash' = fromJust $ maybeLitWord trace._traceContract._codehash
            maybeContractName = maybeContractNameFromCodeHash codehash'
        in
        case trace._traceData of
          EventTrace addr bytes topics ->
            case maybeLitWord =<< listToMaybe topics of
              Nothing   -> []
              Just word -> case M.lookup word eventMap of
                             Just (Event name _ types) ->
                               -- TODO this is where indexed types are filtered out
                               -- they are filtered out for a reason as they only contain
                               -- the topic hash which is printed super verbose by dapptools
                               [name <>
                                showValues [t | (_, t, NotIndexed) <- types] bytes <>
                                pack " from: " <>
                                maybe mempty (\ x -> x <> pack "@") maybeContractName <>
                                pack (show $ forceLit addr)]
                             Nothing -> [pack $ show word]
          ErrorTrace e ->
            case e of
              Revert out -> ["merror " <> "Revert " <> showError out <> maybe mempty (\ x -> pack " from: " <> x) maybeContractName]
              _ -> ["merror " <> pack (show e)]

          _ -> []
  in decodeRevert decodeErrors vm ++ concat (concatMap flatten $ fmap (fmap showTrace) forest)


decodeRevert :: Bool -> VM -> Events
decodeRevert decodeErrors vm =
  case vm._result of
    Just (VMFailure (Revert (ConcreteBuf bs))) -> decodeRevertMsg decodeErrors bs
    _                            -> []

decodeRevertMsg :: Bool -> BS.ByteString -> Events
decodeRevertMsg decodeErrors bs =
  case BS.splitAt 4 bs of
    ("\x08\xc3\x79\xa0",d) | decodeErrors -> ["Error" <> (pack . show $ decodeAbiValue (AbiTupleType (fromList [AbiStringType])) (fromStrict d))]
    ("\x4e\x48\x7b\x71",d)                -> ["Panic(" <> (pack . show $ decodeAbiValue (AbiUIntType 256) (fromStrict d)) <> ")"]
    _                                     -> []
