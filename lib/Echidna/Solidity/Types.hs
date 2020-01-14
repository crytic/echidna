module Echidna.Solidity.Types where

import Data.Text (Text)
import EVM.ABI   (AbiType, AbiValue)
import EVM.Types (Addr)

-- | Represents the type of a Solidity function.
-- A tuple of 'Text' for the name of the function, and then the 'AbiType's of any arguments it expects.
type SolSignature = (Text, [AbiType])

-- | Represents a call to a Solidity function.
-- A tuple of 'Text' for the name of the function, and then any 'AbiValue' arguments passed (as a list).
type SolCall     = (Text, [AbiValue])

-- | An Echidna test is either the name of the function to call and the address where its contract is,
-- or a function that could experience an exception
type SolTest = Either (Text, Addr) SolSignature
