{-# LANGUAGE DeriveGeneric, FlexibleContexts, TemplateHaskell #-}

module Echidna.Property where

--import Echidna.Check (checkETest, checkRTest)
import Control.Lens
import GHC.Generics

import Data.Aeson

data PropertyType = ShouldReturnTrue | ShouldReturnFalse | ShouldRevert 
  deriving (Show, Generic)

--checkTest PropertyType -> 
makeLenses ''PropertyType

instance FromJSON PropertyType
