{-# LANGUAGE ImplicitParams #-}

module Echidna.Events where

import Data.ByteString.Lazy (fromStrict)
import Data.Tree        (flatten)
import Data.Tree.Zipper (fromForest, TreePos, Empty)
import Data.Text        (pack, Text)
import Data.Maybe       (listToMaybe)
import Control.Lens
import EVM
import EVM.ABI      (Event(..), Indexed(..), decodeAbiValue, AbiType(AbiUIntType))
import EVM.Concrete (wordValue)
import EVM.Dapp
import EVM.Format   (showValues, showError)
import EVM.Types    (W256, maybeLitWord)

import qualified Data.Map as M
import qualified Data.ByteString as BS

type EventMap = M.Map W256 Event
type Events = [Text]

emptyEvents :: TreePos Empty a
emptyEvents = fromForest []

extractEvents :: EventMap -> VM -> Events
extractEvents em vm =
  let forest = traceForest vm
      showTrace trace =
        let ?context = DappContext { _contextInfo = emptyDapp, _contextEnv = vm ^?! EVM.env } in
        case view traceData trace of
          EventTrace (Log _ bytes topics) ->
            case maybeLitWord =<< listToMaybe topics of
              Nothing   -> []
              Just word -> case M.lookup (wordValue word) em of
                             Just (Event name _ types) ->
                               [name <> showValues [t | (t, NotIndexed) <- types] bytes]
                             Nothing -> [pack $ show word]
          ErrorTrace e ->
            case e of
              Revert out -> ["merror " <> "Revert " <> showError out]
              _ -> ["merror " <> pack (show e)]
          
          _ -> []
  in decodeRevert vm ++ concat (concatMap flatten $ fmap (fmap showTrace) forest)


decodeRevert :: VM -> Events
decodeRevert vm = 
  case vm ^. result of
    Just (VMFailure (Revert bs)) -> decodeRevertMsg bs
    _                            -> [] 

decodeRevertMsg :: BS.ByteString -> Events 
decodeRevertMsg bs = case BS.splitAt 4 bs of
                          --"\x08\xc3\x79\xa0" -> Just $ "Error(" ++ (show $ decodeAbiValue AbiStringType (fromStrict $ BS.drop 4 bs)) ++ ")"
                          ("\x4e\x48\x7b\x71",d) -> ["Panic(" <> (pack . show $ decodeAbiValue (AbiUIntType 256) (fromStrict d)) <> ")"]
                          _                      -> []
