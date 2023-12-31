{-# LANGUAGE RecordWildCards #-}

module Echidna.Processor where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson ((.:), (.:?), (.!=), eitherDecode, parseJSON, withEmbeddedJSON, withObject)
import Data.Aeson.Types (FromJSON, Parser, Value(String))
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.ByteString.UTF8 qualified as BSU
import Data.Either (fromRight)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.SemVer (Version, fromText)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack)
import System.Directory (findExecutable, listDirectory)
import System.Exit (ExitCode(..))
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import Text.Read (readMaybe)

import EVM.ABI (AbiValue(..))
import EVM.Types (Addr(..), FunctionSelector)

import Echidna.ABI (hashSig, makeNumAbiValues, makeArrayAbiValues)
import Echidna.Types.Signature (ContractName, FunctionName)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Utility (measureIO)

-- | Things that can go wrong trying to run a processor. Read the 'Show'
-- instance for more detailed explanations.
data ProcException = ProcessorFailure String String
                   | ProcessorNotFound String String

instance Show ProcException where
  show = \case
    ProcessorFailure p e -> "Error running " ++ p ++ ":\n" ++ e
    ProcessorNotFound p e -> "Cannot find " ++ p ++ " in PATH.\n" ++ e

instance Exception ProcException

-- | This function is used to filter the lists of function names according to the supplied
-- contract name (if any) and returns a list of hashes
filterResults :: Maybe ContractName -> Map ContractName [FunctionName] -> [FunctionSelector]
filterResults (Just c) rs =
  case Map.lookup c rs of
    Nothing -> filterResults Nothing rs
    Just s -> hashSig <$> s
filterResults Nothing rs = hashSig <$> (concat . Map.elems) rs

enhanceConstants :: SlitherInfo -> Set AbiValue
enhanceConstants si =
  Set.fromList . concatMap enh . concat . concat . Map.elems $ Map.elems <$> si.constantValues
  where
    enh (AbiUInt _ n) = makeNumAbiValues (fromIntegral n)
    enh (AbiInt _ n) = makeNumAbiValues (fromIntegral n)
    enh (AbiString s) = makeArrayAbiValues s
    enh v = [v]

-- we loose info on what constants are in which functions
data SlitherInfo = SlitherInfo
  { payableFunctions :: Map ContractName [FunctionName]
  , constantFunctions :: Map ContractName [FunctionName]
  , asserts :: Map ContractName [FunctionName]
  , constantValues  :: Map ContractName (Map FunctionName [AbiValue])
  , generationGraph :: Map ContractName (Map FunctionName [FunctionName])
  , solcVersions :: [Version]
  , fallbackDefined :: [ContractName]
  , receiveDefined :: [ContractName]
  } deriving (Show)

instance FromJSON SlitherInfo where
  parseJSON = withObject "slitherOutput" $ \o -> do
    -- take the value under 'description' through the path - $['results']['printers'][0]['description']
    results <- o .: "results"
    printer <- NE.head <$> results .: "printers" -- there must be at least one printer
    description <- printer .: "description"
    -- description is a JSON string, needs additional parsing
    withEmbeddedJSON "descriptionString" parseDescription (String description)
    where
      parseDescription = withObject "description" $ \o -> do
        payableFunctions <- o .: "payable"
        constantFunctions <- o .: "constant_functions"
        asserts <- o .: "assert"
        fallbackDefined <- o .:? "with_fallback" .!= ["*"]
        receiveDefined <- o .:? "with_receive" .!= ["*"]
        constantValues'
          -- the type annotation is needed
          :: Map ContractName (Map FunctionName [[Maybe AbiValue]])
          <- o .: "constants_used" >>= (traverse . traverse . traverse . traverse) parseConstant
        -- flatten [[AbiValue]], the array probably shouldn't be nested, fix it in Slither
        let constantValues = (fmap . fmap) (catMaybes . concat) constantValues'
        functionsRelations <- o .: "functions_relations"
        generationGraph <-
          (traverse . traverse) (withObject "relations" (.: "impacts")) functionsRelations
        solcVersions' <- o .:? "solc_versions"
        solcVersions <- case mapM (fromText . pack) (fromMaybe [] solcVersions') of
          Left _ -> pure []
          Right versions -> pure versions
        pure SlitherInfo {..}

      parseConstant :: Value -> Parser (Maybe AbiValue)
      parseConstant = withObject "const" $ \o -> do
        v <- o .: "value"
        t <- o .: "type"
        case t of
          'u':'i':'n':'t':x -> pure $ AbiUInt <$> readMaybe x <*> readMaybe v
          'i':'n':'t':x -> pure $ AbiInt <$> readMaybe x <*> readMaybe v
          "string" ->
            pure . Just . AbiString $
              if "0x" `isPrefixOf` v then
                fromRight (BSU.fromString v) $ BS16.decode $ BSU.fromString $ drop 2 v
              else BSU.fromString v

          "address" -> pure $ AbiAddress . Addr <$> readMaybe v

          -- we don't need all the types for now
          _ -> pure Nothing

-- Slither processing
runSlither :: FilePath -> SolConf -> IO SlitherInfo
runSlither fp solConf = do
  path <- findExecutable "slither" >>= \case
    Nothing -> throwM $
      ProcessorNotFound "slither" "You should install it using 'pip3 install slither-analyzer --user'"
    Just path -> pure path

  let args = ["--ignore-compile", "--print", "echidna", "--json", "-"]
             ++ solConf.cryticArgs ++ [fp]
  (ec, out, err) <- measureIO solConf.quiet ("Running slither on " <> fp) $
    readCreateProcessWithExitCode (proc path args) {std_err = Inherit} ""
  case ec of
    ExitSuccess -> decodeSlitherPrinterJSON (BSL.pack out)
    ExitFailure _ -> throwM $ ProcessorFailure "slither" err

decodeSlitherPrinterJSON :: (MonadThrow f) => BSL.ByteString -> f SlitherInfo
decodeSlitherPrinterJSON buffer =
       case eitherDecode buffer of
           Right si -> pure si
           Left msg -> throwM $ ProcessorFailure "slither" ("decoding slither output failed:\n" ++ msg)

loadSlitherInfos :: CampaignConf -> IO SlitherInfo
loadSlitherInfos solConf = case solConf.slitherInfoDir of
    Nothing -> return noInfo
    Just d -> decodeSlitherPrinterJSONBatch d

decodeSlitherPrinterJSONBatch :: FilePath -> IO SlitherInfo
decodeSlitherPrinterJSONBatch d = do
  fs <- filter (".json" `Data.List.isSuffixOf`) <$> listDirectory d
  bbs <- mapM BSL.readFile fs
  sis <- mapM decodeSlitherPrinterJSON bbs
  return $ foldr appendSlitherInfo noInfo sis

appendSlitherInfo :: SlitherInfo -> SlitherInfo -> SlitherInfo
appendSlitherInfo si1 si2 =
    SlitherInfo {
        payableFunctions = Map.union si1.payableFunctions si2.payableFunctions,
        constantFunctions = Map.union si1.constantFunctions si2.constantFunctions,
        asserts = Map.union si1.asserts si2.asserts,
        constantValues = Map.union si1.constantValues si2.constantValues,
        generationGraph = Map.union si1.generationGraph si2.generationGraph,
        solcVersions = si1.solcVersions ++ si2.solcVersions,
        fallbackDefined = si1.fallbackDefined ++ si2.fallbackDefined,
        receiveDefined = si1.receiveDefined ++ si2.receiveDefined
    }

noInfo :: SlitherInfo
noInfo = SlitherInfo mempty mempty mempty mempty mempty [] [] []
