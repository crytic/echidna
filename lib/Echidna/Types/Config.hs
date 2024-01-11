module Echidna.Types.Config where

import Control.Concurrent (Chan)
import Data.Aeson.Key (Key)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Word (Word64)

import EVM.Dapp (DappInfo)
import EVM.Types (Addr, Contract, W256)

import Echidna.SourceMapping (CodehashMap)
import Echidna.Types.Campaign (CampaignConf, CampaignEvent)
import Echidna.Types.Corpus (Corpus)
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Solidity (SolConf)
import Echidna.Types.Test (TestConf, EchidnaTest)
import Echidna.Types.Tx (TxConf)

data OperationMode = Interactive | NonInteractive OutputFormat deriving (Show, Eq)
data OutputFormat = Text | JSON | None deriving (Show, Eq)
data UIConf = UIConf { maxTime       :: Maybe Int
                     , operationMode :: OperationMode
                     }

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Our big glorious global config type, just a product of each local config.,
data EConfig = EConfig
  { campaignConf :: CampaignConf
  , namesConf :: Names
  , solConf :: SolConf
  , testConf :: TestConf
  , txConf :: TxConf
  , uiConf :: UIConf

  , rpcUrl :: Maybe Text
  , rpcBlock :: Maybe Word64
  }

instance Read OutputFormat where
  readsPrec _ =
    \case 't':'e':'x':'t':r -> [(Text, r)]
          'j':'s':'o':'n':r -> [(JSON, r)]
          'n':'o':'n':'e':r -> [(None, r)]
          _ -> []


data EConfigWithUsage = EConfigWithUsage
  { econfig   :: EConfig
  , badkeys   :: Set Key
  , unsetkeys :: Set Key
  }

data Env = Env
  { cfg :: EConfig
  , dapp :: DappInfo

  -- | Shared between all workers. Events are fairly rare so contention is
  -- minimal.
  , eventQueue :: Chan (Int, LocalTime, CampaignEvent)

  , testsRef :: IORef [EchidnaTest]
  , coverageRef :: IORef CoverageMap
  , corpusRef :: IORef Corpus

  , codehashMap :: CodehashMap
  , fetchContractCache :: IORef (Map Addr (Maybe Contract))
  , fetchSlotCache :: IORef (Map Addr (Map W256 (Maybe W256)))
  , chainId :: Maybe W256
  }
