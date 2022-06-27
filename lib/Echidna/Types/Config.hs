{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Types.Config where

import Control.Lens
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Has (Has(..))

import EVM.Dapp (DappInfo)

import Echidna.Types.Campaign (CampaignConf)
import Echidna.Types.Solidity (SolConf)
import Echidna.Types.Tx  (TxConf)
import Echidna.Types.Test  (TestConf)
import Echidna.UI
import Echidna.UI.Report

twoPower64 :: Int
twoPower64 = 2 ^ 64

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
  _badkeys   :: HashSet Text,
  _unsetkeys :: HashSet Text
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
