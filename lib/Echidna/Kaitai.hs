{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.Kaitai where

import Data.Yaml as Y
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import System.Random (randomRIO)
import Data.Word (Word8)
import EVM.ABI (AbiValue(..), AbiType(..), encodeAbiValue, selector, parseTypeName, putAbi)
import Data.Binary.Put (runPut)
import Control.Monad.Random (MonadRandom)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), withText, withArray)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector, toList, fromList)
import qualified Data.Vector as Vector

-- | Kaitai Struct content
data KContent
  = KContentBytes [Word8]
  | KContentSignature Text
  deriving (Show, Eq)

instance FromJSON KContent where
  parseJSON val = withText "KContent" (pure . KContentSignature) val
              <|> withArray "KContent" (fmap (KContentBytes . toList) . traverse parseJSON) val

-- | Kaitai Struct seq item
data KSeq = KSeq
  { kid :: Text
  , ktype :: Maybe Text
  , size :: Maybe Int
  , contents :: Maybe KContent
  } deriving (Show, Eq, Generic)

instance FromJSON KSeq where
  parseJSON = withObject "KSeq" $ \v -> KSeq
    <$> v .: "id"
    <*> v .:? "type"
    <*> v .:? "size"
    <*> v .:? "contents"

-- | Kaitai Struct specification
data KStruct = KStruct
  { kseqs :: [KSeq]
  , function :: Maybe Text
  , param :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON KStruct where
  parseJSON = withObject "KStruct" $ \v -> do
    meta <- v .:? "meta"
    KStruct
      <$> v .: "seq"
      <*> maybe (pure Nothing) (.:? "function") meta
      <*> maybe (pure Nothing) (.:? "param") meta

-- | Simple ABI type parser
parseAbiType :: Text -> Maybe AbiType
parseAbiType = parseTypeName Vector.empty

-- | ABI-encode a list of values
encodeAbiValues :: Vector AbiValue -> BL.ByteString
encodeAbiValues = runPut . putAbi . AbiTuple

-- | Generate random bytestring of a given size
randomBS :: Int -> IO B.ByteString
randomBS len = B.pack <$> mapM (const $ randomRIO (0, 255)) [1..len]

-- | Generate random value based on Kaitai type
genValue :: (MonadRandom m) => (AbiType -> m AbiValue) -> Text -> m B.ByteString
genValue gen t = do
  let abiType = fromMaybe (error $ "Unsupported Kaitai type: " ++ T.unpack t) (parseAbiType t)
  val <- gen abiType
  pure $ encodeAbiValue val

-- | Generate calldata from a function signature
genCalldataFromSignature :: (MonadRandom m) => (AbiType -> m AbiValue) -> Text -> m B.ByteString
genCalldataFromSignature gen sig = do
  let
    typeString = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') sig
    types = if T.null typeString
            then []
            else map (fromMaybe (error "Invalid ABI type in signature") . parseAbiType) (T.splitOn "," typeString)
    funcSelector = selector sig
  vals <- fromList <$> mapM gen types
  pure $ B.concat [funcSelector, BL.toStrict $ encodeAbiValues vals]

-- | Generate calldata from a Kaitai sequence
genCalldata :: (MonadRandom m, MonadIO m) => (AbiType -> m AbiValue) -> [KSeq] -> m B.ByteString
genCalldata gen = fmap B.concat . mapM (genSeq gen)

-- | Generate calldata for a single Kaitai sequence item
genSeq :: (MonadRandom m, MonadIO m) => (AbiType -> m AbiValue) -> KSeq -> m B.ByteString
genSeq gen kseq =
  case kseq.contents of
    Just (KContentBytes c) -> pure (B.pack c)
    Just (KContentSignature sig) -> genCalldataFromSignature gen sig
    Nothing ->
      case kseq.ktype of
        Just t -> genValue gen t
        Nothing ->
          case kseq.size of
            Just s -> liftIO $ randomBS s
            Nothing -> error "Either type or size must be specified for a seq item"

-- | Main function to decode Kaitai YAML and generate calldata
processKaitai :: (MonadRandom m, MonadIO m) => (AbiType -> m AbiValue) -> KStruct -> m (KStruct, B.ByteString)
processKaitai gen kstruct@(KStruct kseqs _ _) = do
  calldata <- genCalldata gen kseqs
  pure (kstruct, calldata)
