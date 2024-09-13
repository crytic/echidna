module Echidna.Types.Corpus where

import Data.Set (Set, size)
import Echidna.Types.Tx (Tx)

-- (set of transaction sequences in corpus, set of transactions that cause reverts (used for RemoveReverts))
type Corpus = (Set (Int, [Tx]), Set Tx)

corpusSize :: Corpus -> Int
corpusSize = size . fst
