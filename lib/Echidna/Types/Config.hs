module Echidna.Types.Config where

import Data.Aeson.Key (Key)
import Data.HashSet (HashSet)

import EVM.Dapp (DappInfo)

import Echidna.Types.Campaign (CampaignConf)
import Echidna.Types.Solidity (SolConf)
import Echidna.Types.Tx  (TxConf)
import Echidna.Types.Test  (TestConf)
import EVM.Types (Addr)

data OperationMode = Interactive | NonInteractive OutputFormat deriving Show
data OutputFormat = Text | JSON | None deriving Show
data UIConf = UIConf { maxTime       :: Maybe Int
                     , operationMode :: OperationMode
                     }

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

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
  }

instance Read OutputFormat where
  readsPrec _ = \case 't':'e':'x':'t':r -> [(Text, r)]
                      'j':'s':'o':'n':r -> [(JSON, r)]
                      'n':'o':'n':'e':r -> [(None, r)]
                      _ -> []


data EConfigWithUsage = EConfigWithUsage
  { econfig   :: EConfig
  , badkeys   :: HashSet Key
  , unsetkeys :: HashSet Key
  }

data Env = Env
  { cfg :: EConfig
  , dapp :: DappInfo
  }
