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
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import EVM (result)

import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y

import Echidna.Campaign
import Echidna.Solidity
import Echidna.Test
import Echidna.UI

data RuntimeState = RuntimeState { _rtSeed :: Int
                                 }
makeLenses ''RuntimeState

-- | Our big glorious global config type, just a product of each local config.,
data EConfig = EConfig { _cConf :: CampaignConf
                       , _nConf :: Names
                       , _sConf :: SolConf
                       , _tConf :: TestConf
                       , _uConf :: UIConf
                       }
makeLenses ''EConfig

newtype EchidnaRuntime = EchidnaRuntime { getConfig :: RuntimeState -> EConfig }

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

instance FromJSON (RuntimeState -> EConfig) where
  parseJSON (Object v) = do
    tc <- do reverts  <- v .:? "reverts"  .!= True
             psender  <- v .:? "psender"  .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
             let good = if reverts then (`elem` [ResTrue, ResRevert]) else (== ResTrue)
             return $ TestConf (good . maybe ResOther classifyRes . view result) (const psender)
    -- monadically get the values needed to construct a campaign conf
    tl    <- v .:? "testLimit"   .!= 10000
    seql  <- v .:? "seqLen"      .!= 100
    shrl  <- v .:? "shrinkLimit" .!= 5000
    seed' <- v .:? "seed"
    -- and define a function to make one given a RuntimeState
    let mkCC s = CampaignConf tl seql shrl Nothing (fromMaybe (s ^. rtSeed) seed')
    let names = const $ const mempty :: Names
        ppc s c _ = runReader (ppCampaign c) (mkCC s, names)
    --style :: Y.Parser (Campaign -> Int -> String)
    fmt <- v .:? "format" .!= ("text" :: String)
    let style s = case fmt of
                       "text" -> ppc s
                       "json" -> flip $ \g ->
                         unpack . encode . set (_Object . at "seed") (Just $ toJSON g) . toJSON;
                       "none" -> const . const $ ""
                       _      -> const . const $ M.fail
                        "unrecognized ui type (should be text, json, or none)"
    sc <- SolConf <$> v .:? "contractAddr"   .!= 0x00a329c0648769a73afac7f9381e08fb43dbea72
                  <*> v .:? "deployer"       .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                  <*> v .:? "sender"         .!= [0x00a329c0648769a73afac7f9381e08fb43dbea70]
                  <*> v .:? "initialBalance" .!= 0xffffffff
                  <*> v .:? "prefix"         .!= "echidna_"
                  <*> v .:? "solcArgs"       .!= ""
                  <*> v .:? "quiet"          .!= False
    db <- v .:? "dashboard" .!= True
    -- we need to use this make style of conf generation for every conf that
    -- uses RuntimeState
    let mkUIC s = UIConf db (style s)
    return $ \s -> EConfig (mkCC s) names sc tc (mkUIC s)
  parseJSON _ = parseJSON (Object mempty)

-- | The default config used by Echidna (see the 'FromJSON' instance for values used).
defaultConfig :: RuntimeState -> EConfig
defaultConfig = either (error "Config parser got messed up :(") id $ Y.decodeEither' ""

-- | Try to parse an Echidna config file, throw an error if we can't.
parseConfig :: (MonadThrow m, MonadIO m) => FilePath -> RuntimeState -> m EConfig
parseConfig f s = (liftIO (BS.readFile f) >>= Y.decodeThrow) <*> pure s

-- | Run some action with the default configuration, useful in the REPL.
withDefaultConfig :: RuntimeState -> ReaderT EConfig m a -> m a
withDefaultConfig s = (`runReaderT` defaultConfig s)

-- | 'withDefaultConfig' but not for transformers
withDefaultConfig' :: RuntimeState -> Reader EConfig a -> a
withDefaultConfig' s = (`runReader` defaultConfig s)
