{-# LANGUAGE TemplateHaskell #-}

module Echidna.Orphans.JSON () where

-- THIS MODULE EXPORTS NO FUNCTIONS
-- IT ONLY EXPORTS ORPHAN INSTANCES

import Prelude hiding (Word, fail)

import Control.Monad.Fail (fail)
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.DoubleWord (Word256, Int256, Word160)
import Data.Text (Text, unpack)
import EVM.ABI (AbiValue, AbiType)
import EVM.Concrete (Word)
import EVM.Types (Addr)
import Text.Read (readMaybe)

readT :: Read a => Text -> Maybe a
readT = readMaybe . unpack

instance ToJSON Word256 where
  toJSON = toJSON . show

instance FromJSON Word256 where
  parseJSON = withText "Word256" $ maybe (fail "could not parse Word256") pure . readT

instance ToJSON Int256 where
  toJSON = toJSON . show

instance FromJSON Int256 where
  parseJSON = withText "Int256" $ maybe (fail "could not parse Int256") pure . readT

instance ToJSON Word160 where
  toJSON = toJSON . show

instance FromJSON Word160 where
  parseJSON = withText "Int160" $ maybe (fail "could not parse Word160") pure . readT

instance ToJSON ByteString where
  toJSON = toJSON . show

instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ maybe (fail "could not parse ByteString") pure . readT

instance ToJSON Addr where
  toJSON = toJSON . show

instance ToJSON Word where
  toJSON = toJSON . show

instance FromJSON Word where
  parseJSON = withText "Word" $ maybe (fail "could not parse Word") pure . readT

$(deriveJSON defaultOptions ''AbiType)
$(deriveJSON defaultOptions ''AbiValue)
