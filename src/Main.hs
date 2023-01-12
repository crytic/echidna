{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens hiding (argument)
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Data.Aeson.Key qualified as Aeson.Key
import Data.List.NonEmpty qualified as NE
import Data.Map (fromList)
import Data.Maybe (fromMaybe)
import Data.Set qualified as DS
import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Version (showVersion)
import EVM.Types (Addr)
import Options.Applicative
import Paths_echidna (version)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import EVM.Dapp (dappInfo)
import EVM.Solidity (contractName)

import Echidna
import Echidna.Config
import Echidna.Types.Config
import Echidna.Types.Solidity
import Echidna.Types.Campaign
import Echidna.Types.Test (TestMode, testReproducer)
import Echidna.Test (validateTestMode)
import Echidna.Campaign (isSuccess)
import Echidna.UI
import Echidna.Output.Source
import Echidna.Output.Corpus

data Options = Options
  { cliFilePath         :: NE.NonEmpty FilePath
  , cliSelectedContract :: Maybe Text
  , cliConfigFilepath   :: Maybe FilePath
  , cliOutputFormat     :: Maybe OutputFormat
  , cliCorpusDir        :: Maybe FilePath
  , cliTestMode         :: Maybe TestMode
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
                        <> help "Config file (command-line arguments override config options)")
                  <*> optional (option auto $ long "format"
                        <> metavar "FORMAT"
                        <> help "Output format. Either 'json', 'text', 'none'. All these disable interactive UI")
                  <*> optional (option str $ long "corpus-dir"
                        <> metavar "PATH"
                        <> help "Directory to save and load corpus and coverage data.")
                  <*> optional (option str $ long "test-mode"
                        <> help "Test mode to use. Either 'property', 'assertion', 'dapptest', 'optimization', 'overflow' or 'exploration'" )
                  <*> switch (long "multi-abi"
                        <> help "Use multi-abi mode of testing.")
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
  unless cfg._sConf._quiet $
    mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . Aeson.Key.toString) ks
  let cd = cfg._cConf._corpusDir

  (v, sc, cs, w, ts, d, txs) <- prepareContract cfg cliFilePath cliSelectedContract g
  let solcByName = fromList [(c ^. contractName, c) | c <- cs]
  -- TODO put in real path
  let dappInfo' = dappInfo "/" solcByName sc
  let env = Env { cfg = cfg, dapp = dappInfo' }
  -- start ui and run tests
  cpg <- runReaderT (ui v w ts d txs) env

  -- save corpus
  saveTxs (fmap (++ "/reproducers/") cd) (filter (not . null) $ map (^. testReproducer) $ cpg ^. tests)
  saveTxs (fmap (++ "/coverage/") cd) (snd <$> DS.toList (cpg ^. corpus))

  -- get current time to save coverage
  t <- getSystemTime
  let s = fromIntegral $ systemSeconds t
  -- save source coverage
  saveCoverage False s cd sc cs (cpg ^. coverage)
  saveCoverage True  s cd sc cs (cpg ^. coverage)

  if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess

overrideConfig :: EConfig -> Options -> EConfig
overrideConfig config Options{..} =
  foldl (\a f -> f a) config [ overrideFormat
                             , overrideCorpusDir
                             , overrideTestMode
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
      case maybe cfg._uConf.operationMode NonInteractive cliOutputFormat of
        Interactive -> cfg
        NonInteractive Text -> cfg { _uConf = cfg._uConf { operationMode = NonInteractive Text }}
        nonInteractive -> cfg { _uConf = cfg._uConf { operationMode = nonInteractive }
                              , _sConf = cfg._sConf { _quiet = True }
                              }

    overrideCorpusDir cfg =
      cfg & cConf . corpusDir %~ (cliCorpusDir <|>)

    overrideTestMode cfg =
      cfg & sConf . testMode %~ (`fromMaybe` (validateTestMode <$> cliTestMode))

    overrideMultiAbi cfg =
      if cliMultiAbi then cfg & sConf . multiAbi .~ True else cfg

    overrideTestLimit cfg =
      cfg & cConf . testLimit %~ (`fromMaybe` cliTestLimit)

    overrideShrinkLimit cfg =
      cfg & cConf . shrinkLimit %~ (`fromMaybe` cliShrinkLimit)

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
