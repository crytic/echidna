{-|
Module: Echidna.Onchain.Sourcify
Description: Fetch verified contract source code from Sourcify

This module provides functions to fetch verified contract source code,
ABIs, and metadata from Sourcify (sourcify.dev). Sourcify provides
richer metadata than Etherscan including:

- Multiple source files
- Full ABI (functions, events, errors)
- Source maps in JSON (no HTML scraping)
- Immutable references
- Storage layout
-}
module Echidna.Onchain.Sourcify
  ( fetchContractSource
  )
where

import Control.Exception (catch, SomeException)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (parseEither, Parser)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpJSON, parseRequest, getResponseBody, getResponseStatus)
import Network.HTTP.Types.Status (statusCode)

import EVM.Solidity (Reference(..))
import EVM.Types (Addr, W256)

import Echidna.Onchain.Types (SourceData(..))

-- | Sourcify API response structure
data SourcifyResponse = SourcifyResponse
  { sources :: Map Text Text
  , runtimeBytecode :: Maybe BytecodeMeta
  , creationBytecode :: Maybe BytecodeMeta
  , abi :: Maybe [Value]
  , metadata :: Maybe Value                -- ^ Full metadata (contains contract name)
  } deriving (Show, Generic)

instance FromJSON SourcifyResponse where
  parseJSON = withObject "SourcifyResponse" $ \v -> SourcifyResponse
    <$> v .: "sources"
    <*> v .:? "runtimeBytecode"
    <*> v .:? "creationBytecode"
    <*> v .:? "abi"
    <*> v .:? "metadata"

data BytecodeMeta = BytecodeMeta
  { sourceMap :: Maybe Text
  , immutableReferences :: Maybe (Map Text [Reference])
  } deriving (Show, Generic)

instance FromJSON BytecodeMeta where
  parseJSON = withObject "BytecodeMeta" $ \v -> BytecodeMeta
    <$> v .:? "sourceMap"
    <*> v .:? "immutableReferences"

-- | Fetch contract source from Sourcify (always uses https://sourcify.dev)
fetchContractSource
  :: W256   -- ^ chainId
  -> Addr   -- ^ address
  -> IO (Maybe SourceData)
fetchContractSource chainId addr = do
  let baseUrl = "https://sourcify.dev"
      reqUrl = baseUrl <> "/v2/contract/"
                      <> show (fromIntegral chainId :: Integer)
                      <> "/" <> show addr
                      <> "?fields=all"

  putStr $ "Trying Sourcify... "

  catch
    (do
      req <- parseRequest reqUrl
      resp <- httpJSON req
      let status = statusCode $ getResponseStatus resp
      case status of
        200 -> do
          case parseSourcifyResponse (getResponseBody resp) of
            Right srcData -> do
              putStrLn "Success!"
              pure $ Just srcData
            Left err -> do
              putStrLn $ "Parse error: " <> err
              pure Nothing
        404 -> do
          putStrLn "Not verified on Sourcify"
          pure Nothing
        _ -> do
          putStrLn $ "HTTP error: " <> show status
          pure Nothing
    )
    (\(e :: SomeException) -> do
      putStrLn $ "Network error: " <> show e
      pure Nothing
    )

-- | Parse Sourcify JSON response into SourceData
parseSourcifyResponse :: Value -> Either String SourceData
parseSourcifyResponse = parseEither $ \obj -> do
  srcResp <- parseJSON obj :: Parser SourcifyResponse

  -- Extract contract name from metadata
  contractName' <- case srcResp.metadata of
    Just (Object metaObj) -> do
      settings <- metaObj .: "settings"
      compilationTarget <- settings .: "compilationTarget"
      case compilationTarget of
        Object target -> do
          -- compilationTarget is {"Contract.sol": "ContractName"}
          -- We want the value (contract name)
          case KM.elems target of
            [] -> mzero
            (String name:_) -> pure name
            _ -> mzero
        _ -> mzero
    _ -> mzero

  -- Extract runtime source map
  let runtimeSrcMap' = srcResp.runtimeBytecode >>= (.sourceMap)

  -- Extract creation source map
  let creationSrcMap' = srcResp.creationBytecode >>= (.sourceMap)

  -- Extract immutable references and convert keys from Text to W256
  let immutableRefs' = case srcResp.runtimeBytecode of
        Just bytecode -> case bytecode.immutableReferences of
          Just refs -> Just $ Map.mapKeys textToW256 refs
          Nothing -> Nothing
        Nothing -> Nothing

  pure $ SourceData
    { sourceFiles = srcResp.sources
    , runtimeSrcMap = runtimeSrcMap'
    , creationSrcMap = creationSrcMap'
    , contractName = contractName'
    , abi = srcResp.abi
    , immutableRefs = immutableRefs'
    }
  where
    -- Convert Text to W256 (immutable references keys are numeric strings)
    textToW256 :: Text -> W256
    textToW256 t = read (T.unpack t)
