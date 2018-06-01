{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Echidna.Config where

import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson


data Config = Config
  { _solcArgs :: Maybe String
  , _epochs :: Int
  , _gasLimit :: Int
  , _testLimit :: Int
  , _range :: Int }
  deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config

defaultConfig :: Config
defaultConfig = Config
  { _solcArgs = Nothing
  , _epochs = 0
  , _gasLimit = 0xffffffffffffffff
  , _testLimit = 10000
  , _range = 10 }

parseConfig :: FilePath -> IO Config
parseConfig file = do
    content <- BS.readFile file
    let parsedContent = Y.decode content :: Maybe Config
    case parsedContent of
        Nothing -> error "Could not parse config file."
        (Just c) -> return c  
