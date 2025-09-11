{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.Kaitai where

import Data.Yaml as Y
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as B
import GHC.Generics
import System.Random (randomRIO, newStdGen)
import Data.Word (Word8)
import qualified Control.Monad.Random.Strict as Random
import Echidna.ABI (genAbiValueM, GenDict)
import EVM.ABI (AbiType(..), encodeAbiValue)

-- | Kaitai Struct seq item
data KSeq = KSeq
  { kid :: Text
  , ktype :: Maybe Text
  , size :: Maybe Int
  , contents :: Maybe [Word8]
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
  } deriving (Show, Eq, Generic)

instance FromJSON KStruct where
  parseJSON = withObject "KStruct" $ \v -> KStruct
    <$> v .: "seq"

-- | Generate random bytestring of a given size
randomBS :: Int -> IO B.ByteString
randomBS len = B.pack <$> mapM (const $ randomRIO (0, 255)) [1..len]

-- | Generate random value based on Kaitai type
genValue :: GenDict -> Text -> Maybe Int -> IO B.ByteString
genValue dict t _ = do
  g <- newStdGen
  let abiType = case t of
        "address" -> AbiAddressType
        "uint256" -> AbiUIntType 256
        "str" -> AbiStringType
        _ -> error $ "Unsupported Kaitai type: " ++ T.unpack t
  let val = Random.evalRand (genAbiValueM dict abiType) g
  pure $ encodeAbiValue val

-- | Generate calldata from a Kaitai sequence
genCalldata :: GenDict -> [KSeq] -> IO B.ByteString
genCalldata dict = fmap B.concat . mapM (genSeq dict)

-- | Generate calldata for a single Kaitai sequence item
genSeq :: GenDict -> KSeq -> IO B.ByteString
genSeq dict KSeq{..} =
  case contents of
    Just c -> pure (B.pack c)
    Nothing ->
      case ktype of
        Just t -> genValue dict t size
        Nothing ->
          case size of
            Just s -> randomBS s
            Nothing -> error "Either type or size must be specified for a seq item"

-- | Main function to decode Kaitai YAML and generate calldata
processKaitai :: GenDict -> FilePath -> IO B.ByteString
processKaitai dict f = do
  yaml <- Y.decodeFileEither f
  case (yaml :: Either Y.ParseException KStruct) of
    Left err -> error $ "Failed to parse YAML: " ++ show err
    Right (KStruct kseqs) -> genCalldata dict kseqs
