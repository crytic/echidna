{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Version (showVersion)
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
  , cliCheckAsserts     :: Maybe Bool
  }

options :: Parser Options
options = Options <$> (NE.fromList <$> some (argument str (metavar "FILES"
                        <> help "Solidity files to analyze")))
                  <*> optional (option str $ long "contract"
                        <> metavar "CONTRACT"
                        <> help "Contract to analyze")
                  <*> optional (option str $ long "config"
                        <> metavar "CONFIG"
                        <> help "Config file")
                  <*> optional (option auto $ long "format"
                        <> metavar "FORMAT"
                        <> help "Output format: json, text, none. Disables interactive UI")
                  <*> optional (option str $ long "corpus-dir"
                        <> metavar "PATH"
                        <> help "Directory to store corpus and coverage data")
                  <*> optional (switch $ long "check-asserts"
                        <> help "Check asserts in the code")

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
      cfg & sConf . checkAsserts %~ (`fromMaybe` cliCheckAsserts)
