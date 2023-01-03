{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Config where

import Control.Lens
import Data.Aeson.Key (Key)
import Data.Has (Has(..))
import Data.HashSet (HashSet)

import EVM.Dapp (DappInfo)

import Echidna.Types.Campaign (CampaignConf)
import Echidna.Types.Solidity (SolConf)
import Echidna.Types.Tx  (TxConf)
import Echidna.Types.Test  (TestConf)
import Echidna.UI
import Echidna.UI.Report

-- | Our big glorious global config type, just a product of each local config.,
data EConfig = EConfig {
  _cConf :: CampaignConf,
  _nConf :: Names,
  _sConf :: SolConf,
  _tConf :: TestConf,
  _xConf :: TxConf,
  _uConf :: UIConf
}

makeLenses ''EConfig

data EConfigWithUsage = EConfigWithUsage {
  _econfig   :: EConfig,
  _badkeys   :: HashSet Key,
  _unsetkeys :: HashSet Key
}

makeLenses ''EConfigWithUsage

instance Has EConfig EConfigWithUsage where
  hasLens = econfig

instance Has CampaignConf EConfig where
  hasLens = cConf

instance Has Names EConfig where
  hasLens = nConf

instance Has SolConf EConfig where
  hasLens = sConf

instance Has TestConf EConfig where
  hasLens = tConf

instance Has TxConf EConfig where
  hasLens = xConf

instance Has UIConf EConfig where
  hasLens = uConf

data Env = Env {
  _cfg :: EConfig,
  _dapp :: DappInfo
}

makeLenses ''Env

instance Has EConfig Env where
  hasLens = cfg

instance Has CampaignConf Env where
  hasLens = cfg . cConf

instance Has Names Env where
  hasLens = cfg . nConf

instance Has SolConf Env where
  hasLens = cfg . sConf

instance Has TestConf Env where
  hasLens = cfg . tConf

instance Has TxConf Env where
  hasLens = cfg . xConf

instance Has UIConf Env where
  hasLens = cfg . uConf

instance Has DappInfo Env where
  hasLens = dapp
