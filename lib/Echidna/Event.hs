module Echidna.Event (
    extractSeqLog
  , displayEvents
  , Events
  ) where

import Data.Map        (Map)
import Data.Sequence   (Seq, foldrWithIndex)
import Data.Text       (Text, drop, take, length)
import EVM
import EVM.ABI      (Event)
import EVM.Concrete (forceConcreteBlob)
import EVM.Types    (W256)
import EVM.Format   (getEvent, formatLog)

type Events = [Text]

extractSeqLog :: Map W256 Event -> Seq Log -> Events
extractSeqLog m ss = foldrWithIndex (\_ t acc -> t++acc) [] $ fmap showLog ss
   where showLog (Log _ b xs) =  map (\x -> removeESC $ formatLog (getEvent x m) (forceConcreteBlob b)) xs
         removeESC s = Data.Text.drop 5 $ Data.Text.take ((Data.Text.length s) - 4) s 

displayEvents :: Events -> String
displayEvents = show
