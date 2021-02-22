module Echidna.Types.Corpus where

import Data.Set (Set)
import Echidna.Types.Tx (Tx)

type Corpus = Set (Integer, [Tx])
