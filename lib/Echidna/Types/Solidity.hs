{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Solidity where

import Control.Exception (Exception)
import Control.Lens
import Data.List.NonEmpty qualified as NE
import Data.SemVer (Version, version, toString)
import Data.Text (Text, unpack)

import EVM.Solidity
import EVM.Types (Addr)

import Echidna.Types.Signature (ContractName)

minSupportedSolcVersion :: Version
minSupportedSolcVersion = version 0 4 25 [] []

data Filter = Blacklist [Text] | Whitelist [Text] deriving Show

-- | Things that can go wrong trying to load a Solidity file for Echidna testing. Read the 'Show'
-- instance for more detailed explanations.
data SolException = BadAddr Addr
                  | CompileFailure String String
                  | SolcReadFailure
                  | NoContracts
                  | TestArgsFound Text
                  | ContractNotFound Text
                  | NoBytecode Text
                  | NoFuncs
                  | NoTests
                  | OnlyTests
                  | ConstructorArgs String
                  | DeploymentFailed Addr Text
                  | SetUpCallFailed
                  | NoCryticCompile
                  | InvalidMethodFilters Filter
                  | OutdatedSolcVersion Version
makePrisms ''SolException

instance Show SolException where
  show = \case
    BadAddr a                -> "No contract at " ++ show a ++ " exists"
    CompileFailure x y       -> "Couldn't compile given file\n" ++ "stdout:\n" ++ x ++ "stderr:\n" ++ y
    SolcReadFailure          -> "Could not read crytic-export/combined_solc.json"
    NoContracts              -> "No contracts found in given file"
    (ContractNotFound c)     -> "Given contract " ++ show c ++ " not found in given file"
    (TestArgsFound t)        -> "Test " ++ show t ++ " has arguments, aborting"
    (NoBytecode t)           -> "No bytecode found for contract " ++ show t
    NoFuncs                  -> "ABI is empty, are you sure your constructor is right?"
    NoTests                  -> "No tests found in ABI"
    OnlyTests                -> "Only tests and no public functions found in ABI"
    (ConstructorArgs s)      -> "Constructor arguments are required: " ++ s
    NoCryticCompile          -> "crytic-compile not installed or not found in PATH. To install it, run:\n   pip install crytic-compile"
    (InvalidMethodFilters f) -> "Applying " ++ show f ++ " to the methods produces an empty list. Are you filtering the correct functions or fuzzing the correct contract?"
    SetUpCallFailed          -> "Calling the setUp() funciton failed (revert, out-of-gas, sending ether to an non-payable constructor, etc.)"
    (DeploymentFailed a t)   -> "Deploying the contract " ++ show a ++ " failed (revert, out-of-gas, sending ether to an non-payable constructor, etc.):\n" ++ unpack t
    OutdatedSolcVersion v    -> "Solc version " ++ toString v ++ " detected. Echidna doesn't support versions of solc before " ++ toString minSupportedSolcVersion ++ ". Please use a newer version."


instance Exception SolException

-- | Configuration for loading Solidity for Echidna testing.
data SolConf = SolConf { _contractAddr    :: Addr             -- ^ Contract address to use
                       , _deployer        :: Addr             -- ^ Contract deployer address to use
                       , _sender          :: NE.NonEmpty Addr -- ^ Sender addresses to use
                       , _balanceAddr     :: Integer          -- ^ Initial balance of deployer and senders
                       , _balanceContract :: Integer          -- ^ Initial balance of contract to test
                       , _codeSize        :: Integer          -- ^ Max code size for deployed contratcs (default 24576, per EIP-170)
                       , _prefix          :: Text             -- ^ Function name prefix used to denote tests
                       , _cryticArgs      :: [String]         -- ^ Args to pass to crytic
                       , _solcArgs        :: String           -- ^ Args to pass to @solc@
                       , _solcLibs        :: [String]         -- ^ List of libraries to load, in order.
                       , _quiet           :: Bool             -- ^ Suppress @solc@ output, errors, and warnings
                       , _initialize      :: Maybe FilePath   -- ^ Initialize world with Etheno txns
                       , _deployContracts :: [(Addr, String)] -- ^ List of contracts to deploy in specific addresses
                       , _deployBytecodes :: [(Addr, Text)]   -- ^ List of contracts to deploy in specific addresses
                       , _multiAbi        :: Bool             -- ^ Whether or not to use the multi-abi mode
                       , _testMode        :: String           -- ^ Testing mode
                       , _testDestruction :: Bool             -- ^ Whether or not to add a property to detect contract destruction
                       , _methodFilter    :: Filter           -- ^ List of methods to avoid or include calling during a campaign
                       }
makeLenses ''SolConf

-- | List of contract names from every source cache
type SourceCaches = [([ContractName], SourceCache)]

defaultContractAddr :: Addr
defaultContractAddr = 0x00a329c0648769a73afac7f9381e08fb43dbea72

defaultDeployerAddr :: Addr
defaultDeployerAddr = 0x30000
