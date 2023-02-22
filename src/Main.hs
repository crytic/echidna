{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandomR)
import Data.Aeson.Key qualified as Aeson.Key
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Version (showVersion)
import Main.Utf8 (withUtf8)
import Options.Applicative
import Paths_echidna (version)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.CodePage (withCP65001)

import EVM.Dapp (dappInfo)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Addr)

import Echidna
import Echidna.Config
import Echidna.Types.Config
import Echidna.Types.Solidity
import Echidna.Types.Campaign
import Echidna.Types.Test (TestMode, EchidnaTest(..))
import Echidna.Test (validateTestMode)
import Echidna.Campaign (isSuccessful)
import Echidna.UI
import Echidna.Output.Source
import Echidna.Output.Corpus

main :: IO ()
main = withUtf8 $ withCP65001 $ do
  opts@Options{..} <- execParser optsParser
  g <- getRandomR (0, maxBound)
  EConfigWithUsage loadedCfg ks _ <-
    maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig cliConfigFilepath
  let cfg = overrideConfig loadedCfg opts
  unless cfg.solConf.quiet $
    mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . Aeson.Key.toString) ks

  (v, sc, cs, w, ts, d, txs) <- prepareContract cfg cliFilePath cliSelectedContract g
  let solcByName = fromList [(c.contractName, c) | c <- cs]
  -- TODO put in real path
  let dappInfo' = dappInfo "/" solcByName sc
  let env = Env { cfg = cfg, dapp = dappInfo' }
  -- start ui and run tests
  cpg <- runReaderT (ui v w ts (Just d) txs) env

  -- save corpus
  case cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      saveTxs (dir </> "reproducers") (filter (not . null) $ (.testReproducer) <$> cpg._tests)
      saveTxs (dir </> "coverage") (snd <$> Set.toList cpg._corpus)

      -- TODO: add another option to config for saving coverage report
      -- get current time to save coverage
      t <- getSystemTime
      let s = fromIntegral $ systemSeconds t
      -- save source coverage
      saveCoverage False s dir sc cs cpg._coverage
      saveCoverage True  s dir sc cs cpg._coverage

  if isSuccessful cpg then exitSuccess else exitWith (ExitFailure 1)

data Options = Options
  { cliFilePath         :: NE.NonEmpty FilePath
  , cliSelectedContract :: Maybe Text
  , cliConfigFilepath   :: Maybe FilePath
  , cliOutputFormat     :: Maybe OutputFormat
  , cliCorpusDir        :: Maybe FilePath
  , cliTestMode         :: Maybe TestMode
  , cliAllContracts     :: Bool
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

optsParser :: ParserInfo Options
optsParser = info (helper <*> versionOption <*> options) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

options :: Parser Options
options = Options
  <$> (NE.fromList <$> some (argument str (metavar "FILES"
    <> help "Solidity files to analyze")))
  <*> optional (option str $ long "contract"
    <> metavar "CONTRACT"
    <> help "Contract to analyze")
  <*> optional (option str $ long "config"
    <> metavar "CONFIG"
    <> help "Config file (command-line arguments override config options)")
  <*> optional (option auto $ long "format"
    <> metavar "FORMAT"
    <> help "Output format. Either 'json', 'text', 'none'. All these disable interactive UI")
  <*> optional (option str $ long "corpus-dir"
    <> metavar "PATH"
    <> help "Directory to save and load corpus and coverage data.")
  <*> optional (option str $ long "test-mode"
    <> help "Test mode to use. Either 'property', 'assertion', 'dapptest', 'optimization', 'overflow' or 'exploration'" )
  <*> switch (long "all-contracts"
    <> help "Generate calls to all deployed contracts.")
  <*> optional (option auto $ long "test-limit"
    <> metavar "INTEGER"
    <> help ("Number of sequences of transactions to generate during testing. Default is " ++ show defaultTestLimit))
  <*> optional (option auto $ long "shrink-limit"
    <> metavar "INTEGER"
    <> help ("Number of tries to attempt to shrink a failing sequence of transactions. Default is " ++ show defaultShrinkLimit))
  <*> optional (option auto $ long "seq-len"
    <> metavar "INTEGER"
    <> help ("Number of transactions to generate during testing. Default is " ++ show defaultSequenceLength))
  <*> optional (option auto $ long "contract-addr"
    <> metavar "ADDRESS"
    <> help ("Address to deploy the contract to test. Default is " ++ show defaultContractAddr))
  <*> optional (option auto $ long "deployer"
    <> metavar "ADDRESS"
    <> help ("Address of the deployer of the contract to test. Default is " ++ show defaultDeployerAddr))
  <*> many (option auto $ long "sender"
    <> metavar "ADDRESS"
    <> help "Addresses to use for the transactions sent during testing. Can be passed multiple times. Check the documentation to see the default values.")
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

overrideConfig :: EConfig -> Options -> EConfig
overrideConfig config Options{..} =
  config { solConf = overrideSolConf config.solConf
         , campaignConf = overrideCampaignConf config.campaignConf
         }
         & overrideFormat
  where
    overrideFormat cfg =
      case maybe cfg.uiConf.operationMode NonInteractive cliOutputFormat of
        Interactive -> cfg
        NonInteractive Text -> cfg { uiConf = cfg.uiConf { operationMode = NonInteractive Text }}
        nonInteractive -> cfg { uiConf = cfg.uiConf { operationMode = nonInteractive }
                              , solConf = cfg.solConf { quiet = True }
                              }

    overrideCampaignConf campaignConf = campaignConf
      { corpusDir = cliCorpusDir <|> campaignConf.corpusDir
      , testLimit = fromMaybe campaignConf.testLimit cliTestLimit
      , shrinkLimit = fromMaybe campaignConf.shrinkLimit cliShrinkLimit
      , seqLen = fromMaybe campaignConf.seqLen cliSeqLen
      , seed = cliSeed <|> campaignConf.seed
      }

    overrideSolConf solConf = solConf
      { solcArgs = fromMaybe solConf.solcArgs cliSolcArgs
      , cryticArgs = maybe solConf.cryticArgs words cliCryticArgs
      , sender = if null cliSender then solConf.sender else Set.fromList cliSender
      , deployer = fromMaybe solConf.deployer cliDeployer
      , contractAddr = fromMaybe solConf.contractAddr cliContractAddr
      , testMode = maybe solConf.testMode validateTestMode cliTestMode
      , allContracts = cliAllContracts || solConf.allContracts
      }
