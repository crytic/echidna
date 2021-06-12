module Echidna.Events where
import Data.Tree        (flatten)
import Data.Tree.Zipper (fromForest, TreePos, Empty)
import Data.Text        (pack, Text)
import Data.Maybe       (listToMaybe)
import Control.Lens     (view)
import EVM
import EVM.ABI      (Event(..), Indexed(..) )
import EVM.Types    (W256)
import EVM.Concrete (wordValue)
import EVM.Format   (showValues, showError)
import EVM.Symbolic (maybeLitWord)

import qualified Data.Map as M

type EventMap = M.Map W256 Event
type Events = [Text]

emptyEvents :: TreePos Empty a
emptyEvents = fromForest []

extractEvents :: EventMap -> VM -> Events
extractEvents em vm = 
  let forest = traceForest vm 
      showTrace trace = 
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
  in concat $ concatMap flatten $ fmap (fmap showTrace) forest 
