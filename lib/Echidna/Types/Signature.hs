module Echidna.Types.Signature where

import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import EVM.ABI (AbiType, AbiValue)
import EVM.Types (Addr)
import GHC.Word (Word32)

-- | Name of the contract
type ContractName = Text

-- | Name of a function
type FunctionName = Text

type FunctionHash = Word32

-- | Represents the type of a Solidity function.
-- A tuple for the name of the function and the 'AbiType's of any arguments it expects.
type SolSignature = (FunctionName, [AbiType])

-- | Represents a call to a Solidity function.
-- A tuple for the name of the function and then any 'AbiValue' arguments passed (as a list).
type SolCall     = (FunctionName, [AbiValue])

-- | A contract is just an address with an ABI (for our purposes).
type ContractA = (Addr, NonEmpty SolSignature)

type SignatureMap = HashMap Int (NonEmpty SolSignature)
