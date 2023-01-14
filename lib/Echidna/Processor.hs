{-# LANGUAGE RecordWildCards #-}

module Echidna.Processor where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow(..))
import Data.Aeson ((.:), (.:?), (.!=), decode, parseJSON, withEmbeddedJSON, withObject)
import Data.Aeson.Types (FromJSON, Parser, Value(String))
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.ByteString.UTF8 qualified as BSU
import Data.Containers.ListUtils (nubOrd)
import Data.Either (fromRight)
import Data.HashMap.Strict qualified as M
import Data.List (isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe)
import Data.SemVer (Version, fromText)
import Data.Text (pack, isSuffixOf)
import System.Directory (findExecutable)
import System.Process (StdStream(..), readCreateProcessWithExitCode, proc, std_err)
import System.Exit (ExitCode(..))
import Text.Read (readMaybe)

import EVM.ABI (AbiValue(..))
import EVM.Types (Addr(..))

import Echidna.ABI (hashSig, makeNumAbiValues, makeArrayAbiValues)
import Echidna.Types.Signature (ContractName, FunctionName, FunctionHash)

-- | Things that can go wrong trying to run a processor. Read the 'Show'
-- instance for more detailed explanations.
data ProcException = ProcessorFailure String String
                   | ProcessorNotFound String String

instance Show ProcException where
  show = \case
    ProcessorFailure p e -> "Error running " ++ p ++ ":\n" ++ e
    ProcessorNotFound p e -> "Cannot find " ++ p ++ "in PATH.\n" ++ e

instance Exception ProcException

-- | This function is used to filter the lists of function names according to the supplied
-- contract name (if any) and returns a list of hashes
filterResults :: Maybe ContractName -> M.HashMap ContractName [FunctionName] -> [FunctionHash]
filterResults (Just c) rs =
  case M.lookup c rs of
    Nothing -> filterResults Nothing rs
    Just s -> hashSig <$> s
filterResults Nothing rs = hashSig <$> (concat . M.elems) rs

enhanceConstants :: SlitherInfo -> [AbiValue]
enhanceConstants si =
  nubOrd . concatMap enh . concat . concat . M.elems $ M.elems <$> si.constantValues
  where
    enh (AbiUInt _ n) = makeNumAbiValues (fromIntegral n)
    enh (AbiInt _ n) = makeNumAbiValues (fromIntegral n)
    enh (AbiString s) = makeArrayAbiValues s
    enh v = [v]

-- we loose info on what constants are in which functions
data SlitherInfo = SlitherInfo
  { payableFunctions :: M.HashMap ContractName [FunctionName]
  , constantFunctions :: M.HashMap ContractName [FunctionName]
  , asserts :: M.HashMap ContractName [FunctionName]
  , constantValues  :: M.HashMap ContractName (M.HashMap FunctionName [AbiValue])
  , generationGraph :: M.HashMap ContractName (M.HashMap FunctionName [FunctionName])
  , solcVersions :: [Version]
  , fallbackDefined :: [ContractName]
  , receiveDefined :: [ContractName]
  } deriving (Show)

noInfo :: SlitherInfo
noInfo = SlitherInfo mempty mempty mempty mempty mempty [] [] []

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
          :: M.HashMap ContractName (M.HashMap FunctionName [[Maybe AbiValue]])
          <- o .: "constants_used" >>= (traverse . traverse . traverse . traverse) parseConstant
        -- flatten [[AbiValue]], the array probably shouldn't be nested, fix it in Slither
        let constantValues = (fmap . fmap) (catMaybes . concat) constantValues'
        functionsRelations <- o .: "functions_relations"
        generationGraph <- (traverse . traverse) (withObject "relations" (.: "impacts")) functionsRelations
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
          'u':'i':'n':'t':x ->
            case AbiUInt <$> readMaybe x <*> readMaybe v of
              Nothing -> failure v t
              i -> pure i

          'i':'n':'t':x ->
            case AbiInt <$> readMaybe x <*> readMaybe v of
              Nothing -> failure v t
              i -> pure i

          "string" ->
            pure . Just . AbiString $
              if "0x" `isPrefixOf` v
              then fromRight (error ("invalid b16 decoding of: " ++ show v)) $ BS16.decode $ BSU.fromString $ drop 2 v
              else BSU.fromString v

          "address" ->
            case AbiAddress . Addr <$> readMaybe v of
              Nothing -> failure v t
              a -> pure a

          -- we don't need all the types for now
          _ -> pure Nothing
        where failure v t = fail $ "failed to parse " ++ t ++ ": " ++ v

-- Slither processing
runSlither :: FilePath -> [String] -> IO SlitherInfo
runSlither fp extraArgs = if ".vy" `isSuffixOf` pack fp then return noInfo else do
  mp <- findExecutable "slither"
  case mp of
    Nothing -> throwM $ ProcessorNotFound "slither" "You should install it using 'pip3 install slither-analyzer --user'"
    Just path -> do
      let args = ["--ignore-compile", "--print", "echidna", "--json", "-"] ++ extraArgs ++ [fp]
      (ec, out, err) <- readCreateProcessWithExitCode (proc path args) {std_err = Inherit} ""
      case ec of
        ExitSuccess ->
          case decode (BSL.pack out) of
            Just si -> pure si
            Nothing -> throwM $ ProcessorFailure "slither" "decoding slither output failed"
        ExitFailure _ -> throwM $ ProcessorFailure "slither" err
