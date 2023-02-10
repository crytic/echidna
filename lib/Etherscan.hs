module Etherscan where

import Control.Exception (catch, SomeException)
import Control.Monad
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Simple (httpSink, parseRequest, getResponseBody, httpJSONEither)
import System.Environment (lookupEnv)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor (attributeIs, content, element, fromDocument, ($//), (&//))

import EVM.Solidity (makeSrcMaps, SrcMap)
import EVM.Types (Addr)

data SourceCode = SourceCode
  { name :: Text
  , code :: String
  }

instance FromJSON SourceCode where
  parseJSON (Object v) = do
    res <- v .: "result"
    case res of
      [Object r] -> SourceCode <$> (r .: "ContractName")
                               <*> (r .: "SourceCode")
      _ -> mzero
  parseJSON _ = mzero

fetchContractSource :: Addr -> IO (Maybe SourceCode)
fetchContractSource addr = do
  apiKey <- lookupEnv "ETHERSCAN_API_KEY"
  url <- parseRequest $ "https://api.etherscan.io/api?"
                        <> "module=contract"
                        <> "&action=getsourcecode"
                        <> "&address=" <> show addr
                        <> maybe "" ("&apikey=" <>) apiKey
  resp <- httpJSONEither url
  case getResponseBody resp of
    Right s -> pure $ Just s
    Left _ -> pure Nothing

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
