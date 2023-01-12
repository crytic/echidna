module Echidna.Types.Corpus where

import Data.Set (Set, size)
import Echidna.Types.Tx (Tx)

type InitialCorpus = (Int, [[Tx]])
type Corpus = Set (Int, [Tx])

corpusSize :: Corpus -> Int
corpusSize = size
