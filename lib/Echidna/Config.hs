{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Echidna.Config where

import Control.Monad.Catch    (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader   (ReaderT, runReaderT)
import Control.Lens
import Control.Exception      (Exception)
import Data.Aeson
import Data.Text              (Text)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

import Echidna.Property (PropertyType(..))

import EVM.Types (Addr, W256)

data Config = Config
  { _solcArgs      :: Maybe String
  , _epochs        :: Int
  , _range         :: Int
  , _contractAddr  :: Addr
  , _sender        :: [Addr]
  , _psender       :: Addr 
  , _addrList      :: Maybe [Addr]
  , _initialValue  :: Int
  , _gasLimit      :: W256 
  , _testLimit     :: Int
  , _shrinkLimit   :: Int
  , _returnType    :: PropertyType
  , _prefix        :: Text
  , _ignored       :: [Text]
  , _payable       :: [Text]
  , _outdir        :: Maybe String
  , _printCoverage :: Bool
  , _outputJson    :: Bool
  , _outputRawTxs  :: Bool
  }
  deriving Show

makeLenses ''Config

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$> v .:? "solcArgs"      .!= Nothing
           <*> v .:? "epochs"        .!= 2
           <*> v .:? "range"         .!= 10
           <*> v .:? "contractAddr"  .!= 0x00a329c0648769a73afac7f9381e08fb43dbea72
           <*> v .:? "sender"        .!= [0x00a329c0648769a73afac7f9381e08fb43dbea70]
           <*> v .:? "psender"       .!= 0x00a329c0648769a73afac7f9381e08fb43dbea70
           <*> v .:? "addrList"      .!= Nothing
           <*> v .:? "initialValue"  .!= 0
           <*> v .:? "gasLimit"      .!= 0xffffffffffffffff
           <*> v .:? "testLimit"     .!= 10000
           <*> v .:? "shrinkLimit"   .!= 1000
           <*> v .:? "returnType"    .!= ShouldReturnTrue
           <*> v .:? "prefix"        .!= "echidna_"
           <*> v .:? "ignored"       .!= []
           <*> v .:? "payable"       .!= []
           <*> v .:? "outdir"        .!= Nothing
           <*> v .:? "printCoverage" .!= False
           <*> v .:? "outputJson"    .!= False
           <*> v .:? "outputRawTxs"  .!= False 
  parseJSON _          = parseJSON (Object mempty)

data ParseException = ParseException FilePath Y.ParseException

defaultConfig :: Config
defaultConfig = either (error "Config parser got messed up :(") id $ Y.decodeEither' ""

instance Show ParseException where
  show (ParseException f err) =
    "Could not parse config file " ++ show f ++ ": " ++ show err

instance Exception ParseException

parseConfig :: (MonadThrow m, MonadIO m) => FilePath -> m Config
parseConfig file = do
  content <- liftIO $ BS.readFile file
  let parsedContent = Y.decodeEither' content
  case parsedContent of
    Left err -> throwM (ParseException file err)
    Right c  -> return c

withDefaultConfig :: ReaderT Config m a -> m a
withDefaultConfig = (`runReaderT` defaultConfig)
