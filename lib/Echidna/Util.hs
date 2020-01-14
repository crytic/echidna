module Echidna.Util where

import Control.Monad (liftM2)
import Data.List (group, sort)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap, fromListWith)
import Data.Text (Text, intercalate)
import Echidna.Solidity.Types (SolSignature)
import EVM.ABI (abiTypeSolidity)

hashMapBy :: (Hashable k, Eq k, Ord a) => (a -> k) -> [a] -> HashMap k [a]
hashMapBy f = fromListWith (++) . mapMaybe (liftM2 fmap (\l x -> (f x, l)) listToMaybe) . group . sort

-- | Get the text signature of a solidity method (for later hashing)
encodeSig :: SolSignature -> Text
encodeSig (n, ts) = n <> "(" <> intercalate "," (abiTypeSolidity <$> ts) <> ")"
