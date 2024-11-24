module Etherscan where

import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Simple (httpSink, parseRequest, getResponseBody, httpJSON)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor (attributeIs, content, element, fromDocument, ($//), (&//))

import EVM.Solidity (makeSrcMaps, SrcMap)
import EVM.Types (Addr)

data SourceCode = SourceCode
  { name :: Text
  , code :: String
  }
  deriving Show

fetchContractSource :: Maybe Text -> Addr -> IO (Maybe SourceCode)
fetchContractSource apiKey addr = do
  url <- parseRequest $ "https://api.etherscan.io/api?"
                        <> "module=contract"
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

-- | Unfortunately, Etherscan doesn't expose source maps in the JSON API.
-- This function scrapes it from the HTML. Return a tuple where the first element
-- is raw srcmap in text format and the second element is a parsed map.
fetchContractSourceMap :: Addr -> IO (Maybe (Text, Seq SrcMap))
fetchContractSourceMap addr = do
  url <- parseRequest $ "https://etherscan.io/address/" <> show addr
  doc <- httpSink url $ const sinkDoc
  let cursor = fromDocument doc
  -- reverse to start looking from the end
  let candidates = reverse $ cursor
        $// attributeIs "id" "dividcode"
        &// element "pre"
        &// content
  -- see which <pre> content parses to a source map
  parsedCandidates <- mapM safeMakeSrcMaps candidates
  -- combine with raw srcmap to return so it's easier to cache
  case catMaybes $ zipWith (\x -> fmap (x,)) candidates parsedCandidates of
    [] -> pure Nothing
    srcmap:_ -> pure (Just srcmap)

-- | Calling makeSrcMaps on arbitrary input is unsafe as it could crash
-- | Wrap it so it doesn't crash, TODO: fix in hevm
safeMakeSrcMaps :: T.Text -> IO (Maybe (Seq SrcMap))
safeMakeSrcMaps x =
  -- $! forces the exception to happen right here so we can catch it
  catch (pure $! makeSrcMaps x) (\(_ :: SomeException) -> pure Nothing)
