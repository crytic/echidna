{-# LANGUAGE DeriveAnyClass #-}

module Echidna.RPC where

import Control.Exception (catch)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey(toJSONKey))
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.HTTP.Simple (HttpException)
import System.Environment
import Text.Read (readMaybe)

import EVM (initialContract)
import EVM.Fetch qualified
import EVM.Types

import Echidna.Orphans.JSON ()
import Echidna.Symbolic (forceWord)
import Echidna.Types (emptyAccount)

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
  catch
    (EVM.Fetch.fetchContractFrom rpcBlock rpcUrl addr)
    (\(_ :: HttpException) -> pure $ Just emptyAccount)

-- TODO: temporary solution, handle errors gracefully
safeFetchSlotFrom :: EVM.Fetch.BlockNumber -> Text -> Addr -> W256 -> IO (Maybe W256)
safeFetchSlotFrom rpcBlock rpcUrl addr slot =
  catch
    (EVM.Fetch.fetchSlotFrom rpcBlock rpcUrl addr slot)
    (\(_ :: HttpException) -> pure $ Just 0)

fetchChainId :: Maybe Text -> IO (Maybe W256)
fetchChainId (Just url) = EVM.Fetch.fetchChainIdFrom url
fetchChainId Nothing = pure Nothing

data FetchedContractData = FetchedContractData
  { runtimeCode :: ByteString
  , nonce :: Maybe W64
  , balance :: W256
  }
  deriving (Generic, ToJSON, FromJSON, Show)

instance ToJSONKey W256 where
  toJSONKey = toJSONKeyText (Text.pack . show)

fromFetchedContractData :: FetchedContractData -> Contract
fromFetchedContractData contractData =
  (initialContract (RuntimeCode (ConcreteRuntimeCode contractData.runtimeCode)))
    { nonce = contractData.nonce
    , balance = Lit contractData.balance
    , external = True
    }

toFetchedContractData :: Contract -> FetchedContractData
toFetchedContractData contract =
  let code = case contract.code of
               RuntimeCode (ConcreteRuntimeCode c) -> c
               _ -> error "unexpected code"
  in FetchedContractData
    { runtimeCode = code
    , nonce = contract.nonce
    , balance = forceWord contract.balance
    }
