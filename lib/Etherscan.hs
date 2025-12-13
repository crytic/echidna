module Etherscan where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, atomicWriteIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpSink, parseRequest, getResponseBody, httpJSON)
import Text.HTML.DOM (sinkDoc)
import Text.Read (readMaybe)
import Text.XML.Cursor (attributeIs, content, element, fromDocument, ($//), (&//))

import EVM.Solidity (makeSrcMaps, SrcMap)
import EVM.Types (Addr, W256)

data SourceCode = SourceCode
  { name :: Text
  , code :: String
  }
  deriving Show

data ChainInfo = ChainInfo
  { chainname :: Text
  , chainid :: Text
  , blockexplorer :: Text
  , apiurl :: Text
  , status :: Int
  } deriving (Show, Generic)

instance FromJSON ChainInfo

newtype ChainlistResponse = ChainlistResponse
  { result :: [ChainInfo]
  } deriving (Show, Generic)

instance FromJSON ChainlistResponse

fetchContractSource :: Maybe W256 -> Maybe Text -> Addr -> IO (Maybe SourceCode)
fetchContractSource chainId apiKey addr = do
  let chainParam = maybe "&chainid=1" (\c -> "&chainid=" <> show (fromIntegral c :: Integer)) chainId
  url <- parseRequest $ "https://api.etherscan.io/v2/api?"
                        <> chainParam
                        <> "&module=contract"
                        <> "&action=getsourcecode"
                        <> "&address=" <> show addr
                        <> T.unpack (maybe "" ("&apikey=" <>) apiKey)
  try url (5 :: Int)
  where
  try url n = do
    resp <- httpJSON url
    let result = getResponseBody resp
    let parsed = flip parseEither result $ \obj -> do
          message :: String <- obj .: "message"
          case message of
            'O':'K':_ -> do
              r <- obj .: "result"
              case r of
                [Object t] -> do
                  sc <- SourceCode <$> (t .: "ContractName")
                                   <*> (t .: "SourceCode")
                  pure $ Right sc
                _ -> mzero
            "NOTOK" -> do
              -- most likely rate limiting
              err :: String <- obj .: "result"
              pure $ Left err
            _ ->
              pure $ Left message
    case join parsed of
      Right src | n > 0 -> pure $ Just src
      Left err | n > 0 -> do
        putStrLn $ "Retrying (" <> show n <> " left). Error: " <> err
        threadDelay (5*1000000 `div` n)
        try url (n - 1)
      _ -> pure Nothing

-- | Fetch the chainlist from Etherscan API and return a map of chainId to block explorer URL
fetchChainlist :: IO (Maybe (Map W256 Text))
fetchChainlist = do
  putStr "Fetching Etherscan chainlist... "
  url <- parseRequest "https://api.etherscan.io/v2/chainlist"
  try url (3 :: Int)
  where
    try url n = catch
      (do
        resp <- httpJSON url
        let result = getResponseBody resp :: ChainlistResponse
        putStrLn "Success!"
        let chainMap = Map.fromList
              [ (cid, T.dropWhileEnd (== '/') ci.blockexplorer)
              | ci <- result.result
              , ci.status == 1  -- Only active chains
              , Just cid <- [readMaybe $ T.unpack ci.chainid]
              ]
        pure $ Just chainMap
      )
      (\(e :: SomeException) -> do
        if n > 0
          then do
            putStrLn $ "Retrying (" <> show n <> " left). Error: " <> show e
            threadDelay 1000000  -- 1 second
            try url (n - 1)
          else do
            putStrLn $ "Failed: " <> show e
            pure Nothing
      )

-- | Get block explorer URL for a chainId, fetching and caching chainlist if needed
getBlockExplorerUrl :: IORef (Maybe (Map W256 Text)) -> W256 -> IO (Maybe Text)
getBlockExplorerUrl cacheRef chainId = do
  cacheVal <- readIORef cacheRef
  case cacheVal of
    Just cache -> pure $ Map.lookup chainId cache
    Nothing -> do
      -- Fetch and populate cache (done at most once)
      maybeChainlist <- fetchChainlist
      atomicWriteIORef cacheRef maybeChainlist
      pure $ maybeChainlist >>= Map.lookup chainId

-- | Unfortunately, Etherscan doesn't expose source maps in the JSON API.
-- This function scrapes it from the HTML. Return a tuple where the first element
-- is raw srcmap in text format and the second element is a parsed map.
fetchContractSourceMap :: IORef (Maybe (Map W256 Text)) -> Maybe W256 -> Addr -> IO (Maybe (Text, Seq SrcMap))
fetchContractSourceMap cacheRef chainId addr = do
  -- Determine block explorer URL
  let defaultUrl = "https://etherscan.io"
  baseUrl <- case chainId of
    Nothing -> pure defaultUrl
    Just cid -> do
      maybeUrl <- getBlockExplorerUrl cacheRef cid
      case maybeUrl of
        Just url -> pure $ T.unpack url
        Nothing -> do
          putStrLn $ "Warning: No block explorer found for chainId "
                  <> show (fromIntegral cid :: Integer) <> ", defaulting to mainnet"
          pure defaultUrl

  -- Scrape HTML from block explorer
  url <- parseRequest $ baseUrl <> "/address/" <> show addr
  doc <- httpSink url $ const sinkDoc
  let cursor = fromDocument doc
  -- reverse to start looking from the end
  let candidates = reverse $ cursor
        $// attributeIs "id" "dividcode"
        &// element "pre"
        &// content
  -- see which <pre> content parses to a source map
  parsedCandidates <- mapM safeMakeSrcMaps candidates
  -- combine with raw srcmap to return so it is easier to cache
  case catMaybes $ zipWith (\x -> fmap (x,)) candidates parsedCandidates of
    [] -> pure Nothing
    srcmap:_ -> pure (Just srcmap)

-- | Calling makeSrcMaps on arbitrary input is unsafe as it could crash
-- | Wrap it so it doesn't crash, TODO: fix in hevm
safeMakeSrcMaps :: T.Text -> IO (Maybe (Seq SrcMap))
safeMakeSrcMaps x =
  -- $! forces the exception to happen right here so we can catch it
  catch (pure $! makeSrcMaps x) (\(_ :: SomeException) -> pure Nothing)
