{-# LANGUAGE RecordWildCards #-}

module Echidna.SourceAnalysis.Slither where

import Data.Aeson ((.:), (.:?), (.!=), eitherDecode, parseJSON, withEmbeddedJSON, withObject)
import Data.Aeson.Types (FromJSON, Parser, Value(String))
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.ByteString.UTF8 qualified as BSU
import Data.Either (fromRight)
import Data.List (isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.SemVer (Version, fromText)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import Text.Read (readMaybe)

import EVM.ABI (AbiValue(..))
import EVM.Types (Addr(..))

import Echidna.ABI (makeNumAbiValues, makeArrayAbiValues)
import Echidna.Types.Signature (ContractName, FunctionName)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Utility (measureIO)
import System.IO (stderr, hPutStrLn)

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
  findExecutable "slither" >>= \case
    Nothing -> do
      hPutStrLn stderr $
        "WARNING: slither not found. Echidna uses Slither (https://github.com/crytic/slither)"
        <> " to perform source analysis, which makes fuzzing more effective. You should install it with"
        <> " 'pip3 install slither-analyzer --user'"
      pure emptySlitherInfo
    Just path -> do
      let args = ["--ignore-compile", "--print", "echidna", "--json", "-"]
                 ++ solConf.cryticArgs ++ [fp]
      (exitCode, out, err) <- measureIO solConf.quiet ("Running slither on " <> fp) $
        readCreateProcessWithExitCode (proc path args) {std_err = Inherit} ""
      case exitCode of
        ExitSuccess ->
          case eitherDecode (BSL.pack out) of
            Right si -> pure si
            Left msg -> do
              hPutStrLn stderr $
                "WARNING: Decoding slither output failed. Echidna will continue,"
                <> " however fuzzing will likely be less effective.\n"
                <> msg
              pure emptySlitherInfo
        ExitFailure _ -> do
          hPutStrLn stderr $
            "WARNING: Running slither failed. Echidna will continue,"
            <> " however fuzzing will likely be less effective.\n"
            <> err
          pure emptySlitherInfo

emptySlitherInfo :: SlitherInfo
emptySlitherInfo = SlitherInfo mempty mempty mempty mempty mempty [] [] []
