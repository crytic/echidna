module Echidna.Types.Signature where

import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import EVM.ABI (AbiType, AbiValue)
import EVM.Types (Addr)
import GHC.Word (Word32)

import qualified Data.ByteString as BS

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

type SignatureMap = HashMap ByteString (NonEmpty SolSignature)

getBytecodeMetadata :: ByteString -> ByteString
getBytecodeMetadata bs =
  let stripCandidates = flip BS.breakSubstring bs <$> knownBzzrPrefixes in
    case find ((/= mempty) . snd) stripCandidates of
      Nothing -> mempty
      Just (_, m) -> m

knownBzzrPrefixes :: [ByteString]
knownBzzrPrefixes = [
  -- a1 65 "bzzr0" 0x58 0x20 (solc <= 0.5.8)
  BS.pack [0xa1, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20],
  -- a2 65 "bzzr0" 0x58 0x20 (solc >= 0.5.9)
  BS.pack [0xa2, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20],
  -- a2 65 "bzzr1" 0x58 0x20 (solc >= 0.5.11)
  BS.pack [0xa2, 0x65, 98, 122, 122, 114, 49, 0x58, 0x20],
  -- a2 64 "ipfs" 0x58 0x22 (solc >= 0.6.0)
  BS.pack [0xa2, 0x64, 0x69, 0x70, 0x66, 0x73, 0x58, 0x22]
  ]
