{-# LANGUAGE ImplicitParams #-}

module Echidna.Events where

import Data.Tree        (flatten)
import Data.Tree.Zipper (fromForest, TreePos, Empty)
import Data.Text        (Text)
import Data.Maybe       (listToMaybe)
import Control.Lens
import EVM
import EVM.ABI      (Event(..), Indexed(..) )
import EVM.Concrete (wordValue)
import EVM.Dapp
import EVM.Format   (showValues)
import EVM.Types    (W256, maybeLitWord)

import qualified Data.Map as M

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
                             Nothing -> []
          _ -> []
  in concat $ concatMap flatten $ fmap (fmap showTrace) forest
