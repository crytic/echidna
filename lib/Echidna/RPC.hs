{-# LANGUAGE DeriveAnyClass #-}

module Echidna.RPC where

import Control.Exception (SomeException)
import Control.Monad.Catch (catchAll)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey(toJSONKey))
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Wreq.Session qualified as Session
import System.Environment
import Text.Read (readMaybe)

import EVM (Contract(..), ContractCode(RuntimeCode), RuntimeCode (..), initialContract)
import EVM.Fetch qualified
import EVM.Types (Addr, W256)

import Echidna.Types (emptyAccount)
import Echidna.Orphans.JSON ()

rpcUrlEnv :: IO (Maybe Text)
rpcUrlEnv = do
  val <- lookupEnv "ECHIDNA_RPC_URL"
  pure (Text.pack <$> val)

rpcBlockEnv :: IO (Maybe Word64)
rpcBlockEnv = do
  val <- lookupEnv "ECHIDNA_RPC_BLOCK"
  pure (val >>= readMaybe)

-- TODO: temporary solution, handle errors gracefully
safeFetchContractFrom :: EVM.Fetch.BlockNumber -> Text -> Addr -> IO (Maybe Contract)
safeFetchContractFrom rpcBlock rpcUrl addr =
  catchAll
    (EVM.Fetch.fetchContractFrom rpcBlock rpcUrl addr)
    (\(_e :: SomeException) -> pure $ Just emptyAccount)

-- TODO: temporary solution, handle errors gracefully
safeFetchSlotFrom :: EVM.Fetch.BlockNumber -> Text -> Addr -> W256 -> IO (Maybe W256)
safeFetchSlotFrom rpcBlock rpcUrl addr slot =
  catchAll
    (EVM.Fetch.fetchSlotFrom rpcBlock rpcUrl addr slot)
    (\(_e :: SomeException) -> pure $ Just 0)

fetchChainId :: Maybe Text -> IO (Maybe W256)
fetchChainId (Just url) = do
  sess <- Session.newAPISession
  EVM.Fetch.fetchQuery
    EVM.Fetch.Latest -- this shouldn't matter
    (EVM.Fetch.fetchWithSession url sess)
    EVM.Fetch.QueryChainId
fetchChainId Nothing = pure Nothing

data FetchedContractData = FetchedContractData
  { runtimeCode :: ByteString
  , nonce :: W256
  , balance :: W256
  }
  deriving (Generic, ToJSON, FromJSON, Show)

instance ToJSONKey W256 where
  toJSONKey = toJSONKeyText (Text.pack . show)

fromFetchedContractData :: FetchedContractData -> Contract
fromFetchedContractData contractData =
  (initialContract (RuntimeCode (ConcreteRuntimeCode contractData.runtimeCode)))
    { _nonce = contractData.nonce
    , _balance = contractData.balance
    , _external = True
    }

toFetchedContractData :: Contract -> FetchedContractData
toFetchedContractData contract =
  let code = case contract._contractcode of
               RuntimeCode (ConcreteRuntimeCode c) -> c
               _ -> error "unexpected code"
  in FetchedContractData
    { runtimeCode = code
    , nonce = contract._nonce
    , balance = contract._balance
    }
