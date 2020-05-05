module Echidna.Events where
import Data.Tree       (flatten)
import Data.Map        (Map, lookup)
import Data.Text       (Text, append)
import Control.Lens    (view)
import EVM
import EVM.ABI      (Event(..), Indexed(..) )
import EVM.Types    (W256)
import EVM.Concrete (wordValue)
import EVM.Format   (showValues)

type EventMap = Map W256 Event
type Events = [Text]

extractEvents :: EventMap -> VM -> Events
extractEvents em vm = 
  let forest = traceForest vm 
      showTrace trace = 
        case view traceData trace of
          EventTrace (Log _ bytes topics) ->
            case topics of
                 [] -> []
                 (topic:_) ->
                   case (Data.Map.lookup (wordValue topic) em) of
                     Just (Event name _ types) -> [name `append` showValues [t | (t, NotIndexed) <- types] bytes ]
                     Nothing -> []
          _ -> []

   in concat $ concatMap flatten $ fmap (fmap showTrace) forest 
