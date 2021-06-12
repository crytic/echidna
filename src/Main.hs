{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Version (showVersion)
import EVM.Types (Addr)
import Options.Applicative
import Paths_echidna (version)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Echidna
import Echidna.Config
import Echidna.Solidity
import Echidna.Types.Campaign
import Echidna.Campaign (isSuccess)
import Echidna.UI
import Echidna.Output.Source
import Echidna.Output.Corpus

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as DS

data Options = Options
  { cliFilePath         :: NE.NonEmpty FilePath
  , cliSelectedContract :: Maybe Text
  , cliConfigFilepath   :: Maybe FilePath
  , cliOutputFormat     :: Maybe OutputFormat
  , cliCorpusDir        :: Maybe FilePath
  , cliCheckAsserts     :: Bool
  , cliMultiAbi         :: Bool
  , cliTestLimit        :: Maybe Int
  , cliShrinkLimit      :: Maybe Int
  , cliSeqLen           :: Maybe Int
  , cliContractAddr     :: Maybe Addr
  , cliDeployer         :: Maybe Addr
  , cliSender           :: [Addr]
  , cliSeed             :: Maybe Int
  , cliCryticArgs       :: Maybe String
  , cliSolcArgs         :: Maybe String
  }

options :: Parser Options
options = Options <$> (NE.fromList <$> some (argument str (metavar "FILES"
                        <> help "Solidity files to analyze")))
                  <*> optional (option str $ long "contract"
                        <> metavar "CONTRACT"
                        <> help "Contract to analyze")
                  <*> optional (option str $ long "config"
                        <> metavar "CONFIG"
                        <> help "Config file (CLI arguments override config options)")
                  <*> optional (option auto $ long "format"
                        <> metavar "FORMAT"
                        <> help "Output format: json, text, none. Disables interactive UI.")
                  <*> optional (option str $ long "corpus-dir"
                        <> metavar "PATH"
                        <> help "Directory to store corpus and coverage data.")
                  <*> switch (long "check-asserts"
                        <> help "Check asserts in the code.")
                  <*> switch (long "multi-abi"
                        <> help "Use multi-abi mode of testing.")
                  <*> optional (option auto $ long "test-limit"
                        <> metavar "INTEGER"
                        <> help "Number of sequences of transactions to generate during testing.")
                  <*> optional (option auto $ long "shrink-limit"
                        <> metavar "INTEGER"
                        <> help "Number of tries to attempt to shrink a failing sequence of transactions.")
                  <*> optional (option auto $ long "seq-len"
                        <> metavar "INTEGER"
                        <> help "Number of transactions to generate during testing.")
                  <*> optional (option auto $ long "contract-addr"
                        <> metavar "ADDRESS"
                        <> help "Address to deploy the contract to test.")
                  <*> optional (option auto $ long "deployer"
                        <> metavar "ADDRESS"
                        <> help "Address of the deployer of the contract to test.")
                  <*> many (option auto $ long "sender"
                        <> metavar "ADDRESS"
                        <> help "Addresses to use for the transactions sent during testing. Can be passed multiple times.")
                  <*> optional (option auto $ long "seed"
                        <> metavar "SEED"
                        <> help "Run with a specific seed.")
                  <*> optional (option str $ long "crytic-args"
                        <> metavar "ARGS"
                        <> help "Additional arguments to use in crytic-compile for the compilation of the contract to test.")
                  <*> optional (option str $ long "solc-args"
                        <> metavar "ARGS"
                        <> help "Additional arguments to use in solc for the compilation of the contract to test.")

versionOption :: Parser (a -> a)
versionOption = infoOption
                  ("Echidna " ++ showVersion version)
                  (long "version" <> help "Show version")

optsParser :: ParserInfo Options
optsParser = info (helper <*> versionOption <*> options) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

main :: IO ()
main = do
  opts@Options{..} <- execParser optsParser
  g <- getRandom
  EConfigWithUsage loadedCfg ks _ <- maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig cliConfigFilepath
  let cfg = overrideConfig loadedCfg opts
  unless (cfg ^. sConf . quiet) $ mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . unpack) ks
  let cd = cfg ^. cConf . corpusDir

  (sc, cs, cpg) <- flip runReaderT cfg $ do
    (v, sc, cs, w, ts, d, txs) <- prepareContract cfg cliFilePath cliSelectedContract g
    -- start ui and run tests
    r <- ui v w ts d txs
    return (sc, cs, r)

  -- save corpus
  saveTxs cd (snd <$> DS.toList (cpg ^. corpus))

  -- save source coverage
  saveCoverage cd sc cs (cpg ^. coverage)

  if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess

overrideConfig :: EConfig -> Options -> EConfig
overrideConfig config Options{..} =
  foldl (\a f -> f a) config [ overrideFormat
                             , overrideCorpusDir
                             , overrideCheckAsserts
                             , overrideMultiAbi
                             , overrideTestLimit
                             , overrideShrinkLimit
                             , overrideSeqLen
                             , overrideContractAddr
                             , overrideDeployer
                             , overrideSender
                             , overrideSeed
                             , overrideCryticArgs
                             , overrideSolcArgs
                             ]
  where
    overrideFormat cfg =
      case maybe (cfg ^. uConf . operationMode) NonInteractive cliOutputFormat of
        Interactive -> cfg
        NonInteractive Text -> cfg & uConf . operationMode .~ NonInteractive Text
        nonInteractive -> cfg & uConf . operationMode .~ nonInteractive
                              & sConf . quiet .~ True

    overrideCorpusDir cfg =
      cfg & cConf . corpusDir %~ (cliCorpusDir <|>)

    overrideCheckAsserts cfg =
      if cliCheckAsserts then cfg & sConf . checkAsserts .~ True else cfg

    overrideMultiAbi cfg =
      if cliMultiAbi then cfg & sConf . multiAbi .~ True else cfg

    overrideTestLimit cfg =
      cfg & cConf . testLimit %~ (`fromMaybe` cliTestLimit)

    overrideShrinkLimit cfg =
      cfg & cConf . testLimit %~ (`fromMaybe` cliShrinkLimit)

    overrideSeqLen cfg =
      cfg & cConf . seqLen %~ (`fromMaybe` cliSeqLen)

    overrideContractAddr cfg =
      cfg & sConf . contractAddr %~ (`fromMaybe` cliContractAddr)

    overrideDeployer cfg =
      cfg & sConf . deployer %~ (`fromMaybe` cliDeployer)

    overrideSender cfg =
      cfg & sConf . sender %~ (`fromMaybe` NE.nonEmpty cliSender)

    overrideSeed cfg =
      cfg & cConf . seed %~ (cliSeed <|>)

    overrideCryticArgs cfg =
      cfg & sConf . cryticArgs %~ (`fromMaybe` (words <$> cliCryticArgs))

    overrideSolcArgs cfg =
      cfg & sConf . solcArgs %~ (`fromMaybe` cliSolcArgs)
