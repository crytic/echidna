module Echidna.Onchain.Types
  ( SourceData(..)
  )
where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)

import EVM.Solidity (Reference)
import EVM.Types (W256)

-- | Unified source data structure for contract source fetching
data SourceData = SourceData
  { sourceFiles :: Map Text Text                  -- filepath -> content
  , runtimeSrcMap :: Maybe Text                   -- Runtime source map string
  , creationSrcMap :: Maybe Text                  -- Creation source map string
  , contractName :: Text                          -- Contract name
  , abi :: Maybe [Value]                          -- Contract ABI (TODO: process it)
  , immutableRefs :: Maybe (Map W256 [Reference]) -- Immutable references
  } deriving Show
