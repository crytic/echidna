{-# LANGUAGE DeriveGeneric, FlexibleContexts, TemplateHaskell #-}

module Echidna.Property where

import Control.Lens
import GHC.Generics

import Data.Aeson

data PropertyType = ShouldReturnTrue | ShouldReturnFalse | ShouldRevert 
  deriving (Show, Generic)

makeLenses ''PropertyType

instance FromJSON PropertyType
