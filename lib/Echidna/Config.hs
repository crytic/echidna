{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Echidna.Config where

import Control.Monad.Catch    (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader   (ReaderT, runReaderT)
import Control.Lens
import Control.Exception      (Exception)
import Data.Aeson

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

import EVM.Types (W256)

data Config = Config
  { _solcArgs :: Maybe String
  , _epochs :: Int
  , _testLimit :: Int
  , _range :: Int
  , _gasLimit :: W256 }
  deriving Show

makeLenses ''Config

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .:? "solcArgs"  .!= Nothing
                                <*> v .:? "epochs"    .!= 2
                                <*> v .:? "testLimit" .!= 10000
                                <*> v .:? "range"     .!= 10
                                <*> v .:? "gasLimit"  .!= 0xffffffffffffffff 
  parseJSON _          = parseJSON (Object mempty)

newtype ParseException = ParseException FilePath

defaultConfig :: Config
defaultConfig = either (error "Config parser got messed up :(") id $ Y.decodeEither ""

instance Show ParseException where
  show (ParseException f) = "Could not parse config file " ++ show f

instance Exception ParseException

parseConfig :: (MonadThrow m, MonadIO m) => FilePath -> m Config
parseConfig file = do
  content <- liftIO $ BS.readFile file
  let parsedContent = Y.decode content :: Maybe Config
  case parsedContent of
    Nothing  -> throwM (ParseException file)
    (Just c) -> return c

withDefaultConfig :: ReaderT Config m a -> m a
withDefaultConfig = (`runReaderT` defaultConfig)
