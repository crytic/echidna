module Echidna.Property where

import Data.Text                           ( unpack )
import Data.Yaml                           ( Value(..), FromJSON, parseJSON )

data PropertyType = ShouldReturnTrue | ShouldReturnFalse | ShouldRevert | ShouldReturnFalseRevert
  deriving (Show)

instance FromJSON PropertyType where
  parseJSON (String "Success")       = pure ShouldReturnTrue
  parseJSON (String "Fail")          = pure ShouldReturnFalse
  parseJSON (String "Throw")         = pure ShouldRevert
  parseJSON (String "Fail or Throw") = pure ShouldReturnFalseRevert
  parseJSON (String s)               = fail $ "Expected return type, not " ++ unpack s
  parseJSON _                        = fail "Expected return type, should be a string"
