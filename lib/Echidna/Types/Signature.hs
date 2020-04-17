module Echidna.Types.Signature where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import EVM.ABI (AbiType, AbiValue)
import EVM.Types (Addr)

-- | Represents the type of a Solidity function.
-- A tuple of 'Text' for the name of the function, and then the 'AbiType's of any arguments it expects.
type SolSignature = (Text, [AbiType])

-- | Represents a call to a Solidity function.
-- A tuple of 'Text' for the name of the function, and then any 'AbiValue' arguments passed (as a list).
type SolCall     = (Text, [AbiValue])

-- | A contract is just an address with an ABI (for our purposes).
type ContractA = (Addr, NonEmpty SolSignature)
