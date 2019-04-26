{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Config where

import Control.Lens
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Has (Has(..))
import Data.Aeson
import Data.Text (pack, isPrefixOf)
import EVM (result)

import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y

import Echidna.Campaign
import Echidna.Solidity
import Echidna.Test
import Echidna.UI

-- | Our big glorious global config type, just a product of each local config.,
data EConfig = EConfig { _cConf :: CampaignConf
                       , _nConf :: Names
                       , _sConf :: SolConf
                       , _tConf :: TestConf
                       , _uConf :: UIConf
                       }
makeLenses ''EConfig

instance Has CampaignConf EConfig where
  hasLens = cConf

instance Has Names EConfig where
  hasLens = nConf

instance Has SolConf EConfig where
  hasLens = sConf

instance Has TestConf EConfig where
  hasLens = tConf

instance Has UIConf EConfig where
  hasLens = uConf

instance FromJSON EConfig where
  parseJSON (Object v) =
    let tc = do psender  <- v .:? "psender"  .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                fprefix <- v .:? "prefix" .!= "echidna_"
                let good fname = if pack (fprefix ++ "revert_") `isPrefixOf` fname then (== ResRevert) else (== ResTrue)
                return $ TestConf (\fname -> good fname . maybe ResOther classifyRes . view result) (const psender)
        cc = CampaignConf <$> v .:? "testLimit"   .!= 10000
                          <*> v .:? "seqLen"      .!= 100
                          <*> v .:? "shrinkLimit" .!= 5000
                          <*> pure Nothing
        names = const $ const mempty :: Names
        ppc = cc <&> \c x -> runReader (ppCampaign x) (c, names)
        style :: Y.Parser (Campaign -> String)
        style = v .:? "format" >>= \case (Nothing :: Maybe String) -> ppc
                                         (Just "text")             -> ppc
                                         (Just "json")             -> pure $ unpack . encode
                                         (Just "none")             -> pure $ const ""
                                         _                         -> M.fail
                                           "unrecognized ui type (should be text, json, or none)" in
    EConfig <$> cc
            <*> pure names
            <*> (SolConf <$> v .:? "contractAddr"   .!= 0x00a329c0648769a73afac7f9381e08fb43dbea72
                         <*> v .:? "deployer"       .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                         <*> v .:? "sender"         .!= [0x00a329c0648769a73afac7f9381e08fb43dbea70]
                         <*> v .:? "initialBalance" .!= 0xffffffff
                         <*> v .:? "prefix"         .!= "echidna_"
                         <*> v .:? "solcArgs"       .!= ""
                         <*> v .:? "quiet"          .!= False)
            <*> tc
            <*> (UIConf <$> v .:? "dashboard" .!= True <*> style)
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

-- | 'withDefaultConfig' but not for transformers
withDefaultConfig' :: Reader EConfig a -> a
withDefaultConfig' = (`runReader` defaultConfig)
