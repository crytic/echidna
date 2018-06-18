{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Echidna.Config where

import Control.Lens
import Data.Aeson
--import Data.DoubleWord (Word256)
import GHC.Generics

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

import EVM.Types (W256)


data Config = Config
  { _solcArgs :: Maybe String
  , _epochs :: Int
  , _testLimit :: Int
  , _range :: Int
  , _gasLimit :: W256 }
  deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config

defaultConfig :: Config
defaultConfig = Config
  { _solcArgs = Nothing
  , _epochs = 2
  , _testLimit = 10000
  , _range = 10
  , _gasLimit = 0xffffffffffffffff }

parseConfig :: FilePath -> IO Config
parseConfig file = do
    content <- BS.readFile file
    let parsedContent = Y.decode content :: Maybe Config
    case parsedContent of
        Nothing -> error "Could not parse config file."
        (Just c) -> return c  
