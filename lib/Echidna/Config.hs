{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Config where

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(..))
import Data.Has (Has(..))
import Data.Aeson
import EVM (result)

import qualified Data.ByteString as BS
import qualified Data.Yaml as Y

import Echidna.Campaign
import Echidna.ABI
import Echidna.Solidity
import Echidna.Test
import Echidna.UI

instance FromJSON UIType

-- | Our big glorious global config type, just a product of each local config.
data EConfig = EConfig { _cConf :: CampaignConf
                       , _gConf :: GenConf
                       , _nConf :: Names
                       , _sConf :: SolConf
                       , _tConf :: TestConf
                       }
makeLenses ''EConfig

instance Has CampaignConf EConfig where
  hasLens = cConf

instance Has GenConf EConfig where
  hasLens = gConf

instance Has Names EConfig where
  hasLens = nConf

instance Has SolConf EConfig where
  hasLens = sConf

instance Has TestConf EConfig where
  hasLens = tConf

instance FromJSON EConfig where
  parseJSON (Object v) =
    let tc = do reverts <- v .:? "reverts"   .!= True
                psender  <- v .:? "psender"  .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                let good = if reverts then (`elem` [ResTrue, ResRevert]) else (== ResTrue)
                return $ TestConf (good . maybe ResOther classifyRes . view result) (const psender) in
    EConfig <$> (CampaignConf <$> v .:? "testLimit"   .!= 10000
                              <*> v .:? "seqLen"      .!= 10
                              <*> v .:? "shrinkLimit" .!= 5000
                              <*> pure Nothing
                              <*> v .:? "ui"          .!= Auto)
            <*> pure (GenConf 0 mempty mempty)
            <*> pure (const $ const mempty)
            <*> (SolConf <$> v .:? "contractAddr" .!= 0x00a329c0648769a73afac7f9381e08fb43dbea72
                         <*> v .:? "deployer"     .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                         <*> v .:? "sender"     .!= [0x00a329c0648769a73afac7f9381e08fb43dbea70]
                         <*> v .:? "prefix  "     .!= "echidna_"
                         <*> v .:? "solcArgs  "   .!= "")
            <*> tc
  parseJSON _ = parseJSON (Object mempty)

-- | The default config used by Echidna (see the 'FromJSON' instance for values used).
defaultConfig :: EConfig
defaultConfig = either (error "Config parser got messed up :(") id $ Y.decodeEither' ""

-- | Try to parse an Echidna config file, throw an error if we can't.
parseConfig :: (MonadThrow m, MonadIO m) => FilePath -> m EConfig
parseConfig f = liftIO (BS.readFile f) >>= Y.decodeThrow

-- | Run some action with the default configuration, useful in the REPL.
withDefaultConfig :: ReaderT EConfig m a -> m a
withDefaultConfig = (`runReaderT` defaultConfig)
