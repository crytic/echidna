{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Util where

import Control.Monad (liftM2)
import Data.List (group, sort)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap, fromListWith)
import Data.Text (Text, intercalate)
import Echidna.Solidity.Types (SolSignature, CallRes(..))
import EVM (VMResult(..), Error(..))
import EVM.ABI (AbiValue(AbiBool), abiTypeSolidity, encodeAbiValue)

hashMapBy :: (Hashable k, Eq k, Ord a) => (a -> k) -> [a] -> HashMap k [a]
hashMapBy f = fromListWith (++) . mapMaybe (liftM2 fmap (\l x -> (f x, l)) listToMaybe) . group . sort

-- | Get the text signature of a solidity method (for later hashing)
encodeSig :: SolSignature -> Text
encodeSig (n, ts) = n <> "(" <> intercalate "," (abiTypeSolidity <$> ts) <> ")"

-- | Broad categories of execution failures: reversions, illegal operations, and ???.
data ErrorClass = RevertE | IllegalE | UnknownE

-- | Given an execution error, classify it. Mostly useful for nice @pattern@s ('Reversion', 'Illegal').
classifyError :: Error -> ErrorClass
classifyError (OutOfGas _ _)         = RevertE
classifyError (Revert _)             = RevertE
classifyError (UnrecognizedOpcode _) = RevertE
classifyError (Query _)              = RevertE
classifyError StackUnderrun          = IllegalE
classifyError BadJumpDestination     = IllegalE
classifyError StackLimitExceeded     = IllegalE
classifyError IllegalOverflow        = IllegalE
classifyError _                      = UnknownE

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | Given a 'VMResult', classify it assuming it was the result of a call to an Echidna test.
classifyRes :: VMResult -> CallRes
classifyRes (VMSuccess b) | b == encodeAbiValue (AbiBool True)  = ResTrue
                          | b == encodeAbiValue (AbiBool False) = ResFalse
                          | otherwise                           = ResOther

classifyRes Reversion = ResRevert
classifyRes _ = ResOther
