module Echidna.Types.Corpus where

import Data.Set (Set, size)
import Echidna.Types.Tx (Tx)

type Corpus = Set (Integer, [Tx])

corpusSize :: Corpus -> Int
corpusSize = size
