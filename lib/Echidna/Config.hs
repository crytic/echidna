{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Config where

import Control.Lens
import Control.Monad (liftM2, liftM5)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (Reader, ReaderT(..), runReader)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Has (Has(..))
import Data.Aeson
import Data.Aeson.Lens
import Data.Functor ((<&>))
import Data.Text (isPrefixOf)
import EVM (result)
import EVM.Concrete (Word(..), Whiff(..))

import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Yaml as Y

import Echidna.Campaign
import Echidna.Solidity
import Echidna.Test
import Echidna.Transaction
import Echidna.UI

-- | Our big glorious global config type, just a product of each local config.,
data EConfig = EConfig { _cConf :: CampaignConf
                       , _nConf :: Names
                       , _sConf :: SolConf
                       , _tConf :: TestConf
                       , _xConf :: TxConf
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

instance Has TxConf EConfig where
  hasLens = xConf

instance Has UIConf EConfig where
  hasLens = uConf

instance FromJSON EConfig where
  parseJSON (Object v) =
    let tc = do psender <- v .:? "psender" .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                fprefix <- v .:? "prefix"  .!= "echidna_"
                let goal fname = if (fprefix <> "revert_") `isPrefixOf` fname then ResRevert else ResTrue
                return $ TestConf (\fname -> (== goal fname)  . maybe ResOther classifyRes . view result)
                                  (const psender)
        getWord s d = C Dull . fromIntegral <$> v .:? s .!= (d :: Integer)
        xc = liftM5 TxConf (getWord "propMaxGas" 8000030) (getWord "testMaxGas" 0xffffffff)
                           (getWord "maxGasprice" 100000000000)
                           (getWord "maxTimeDelay" 604800)     (getWord "maxBlockDelay" 60480)
        cov = v .:? "coverage" <&> \case Just True -> Just mempty
                                         _         -> Nothing
        cc = CampaignConf <$> v .:? "testLimit"   .!= 50000
                          <*> v .:? "stopOnFail"  .!= False
                          <*> v .:? "seqLen"      .!= 100
                          <*> v .:? "shrinkLimit" .!= 5000
                          <*> cov
                          <*> v .:? "seed"
                          <*> v .:? "dictFreq"    .!= 0.40

        names :: Names
        names Sender = (" from: " ++) . show
        names _      = const ""
        ppc :: Y.Parser (Campaign -> Int -> String)
        ppc = liftM2 (\cf xf c g -> runReader (ppCampaign c) (cf, xf, names) ++ "\nSeed: " ++ show g) cc xc
        style :: Y.Parser (Campaign -> Int -> String)
        style = v .:? "format" .!= ("text" :: String) >>=
          \case "text"             -> ppc
                "json"             -> pure . flip $ \g ->
                  unpack . encode . set (_Object . at "seed") (Just . toJSON $ g) . toJSON;
                "none"             -> pure $ \_ _ -> ""
                _                  -> pure $ \_ _ -> M.fail
                  "unrecognized ui type (should be text, json, or none)" in
    EConfig <$> cc
            <*> pure names
            <*> (SolConf <$> v .:? "contractAddr"   .!= 0x00a329c0648769a73afac7f9381e08fb43dbea72
                         <*> v .:? "deployer"       .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
                         <*> v .:? "sender"         .!= NE.fromList [0x10000, 0x20000, 0x00a329c0648769a73afac7f9381e08fb43dbea70]
                         <*> v .:? "balanceAddr"    .!= 0xffffffff
                         <*> v .:? "balanceContract".!= 0
                         <*> v .:? "prefix"         .!= "echidna_"
                         <*> v .:? "cryticArgs"     .!= []
                         <*> v .:? "solcArgs"       .!= ""
                         <*> v .:? "solcLibs"       .!= []
                         <*> v .:? "quiet"          .!= False
                         <*> v .:? "checkAsserts"   .!= False)
            <*> tc
            <*> xc
            <*> (UIConf <$> v .:? "dashboard" .!= True <*> v .:? "timeout" <*> style)
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
