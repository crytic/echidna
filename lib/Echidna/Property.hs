module Echidna.Property where

import qualified Data.HashMap.Lazy as HML  ( lookup )
import Data.Yaml                           ( Value(..), FromJSON, parseJSON )

data PropertyType = ShouldReturnTrue | ShouldReturnFalse | ShouldRevert | ShouldReturnFalseRevert
  deriving (Show)

instance FromJSON PropertyType where
  parseJSON (Object o) = case HML.lookup "value" o of
        Just (String "Sucess")        -> pure ShouldReturnTrue
        Just (String "Fail")          -> pure ShouldReturnFalse
        Just (String "Throw")         -> pure ShouldRevert
        Just (String "Fail or Throw") -> pure ShouldReturnFalseRevert
        _                             -> fail "Expected return type"
  parseJSON _ = fail "Expected return type"
