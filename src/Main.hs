{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (unless, forM_, when)
import Control.Monad.Reader (runReaderT, liftIO)
import Control.Monad.Random (getRandomR)
import Data.Aeson.Key qualified as Aeson.Key
import Data.Char (toLower)
import Data.Function ((&))
import Data.Hashable (hash)
import Data.IORef (readIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Version (showVersion)
import Data.Word (Word8, Word16, Word64)
import Main.Utf8 (withUtf8)
import Options.Applicative
import Paths_echidna (version)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.IO (hPutStrLn, stderr)
import System.IO.CodePage (withCP65001)

import EVM.Dapp (DappInfo(..))
import EVM.Solidity (BuildOutput(..), Contracts(..))
import EVM.Types (Addr)

import Echidna
import Echidna.Campaign (isSuccessful)
import Echidna.Config
import Echidna.Onchain qualified as Onchain
import Echidna.Output.Corpus
import Echidna.Output.Source
import Echidna.Solidity (compileContracts)
import Echidna.Test (reproduceTest, validateTestMode)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Solidity
import Echidna.Types.Test (TestMode, EchidnaTest(..))
import Echidna.UI
import Echidna.UI.Report (ppFailWithTraces, ppTestName)
import Echidna.Utility (measureIO)

main :: IO ()
main = withUtf8 $ withCP65001 $ do
  opts@Options{..} <- execParser optsParser
  EConfigWithUsage loadedCfg ks _ <-
    maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig cliConfigFilepath
  cfg <- overrideConfig loadedCfg opts

  unless cfg.solConf.quiet $
    forM_ ks $ hPutStrLn stderr . ("Warning: unused option: " ++) . Aeson.Key.toString

  buildOutput <- compileContracts cfg.solConf cliFilePath
  env <- mkEnv cfg buildOutput

  Onchain.loadRpcCache env

  -- take the seed from config, otherwise generate a new one
  seed <- maybe (getRandomR (0, maxBound)) pure cfg.campaignConf.seed
  (vm, world, dict) <- prepareContract env cliFilePath cliSelectedContract seed

  initialCorpus <- loadInitialCorpus env world
  let (Contracts contractMap) = buildOutput.contracts
  -- start ui and run tests
  _campaign <- runReaderT (ui vm world dict initialCorpus cliSelectedContract (Map.elems contractMap)) env

  tests <- readIORef env.testsRef

  Onchain.saveRpcCache env

  -- save corpus
  case cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      measureIO cfg.solConf.quiet "Saving test reproducers" $
        saveTxs (dir </> "reproducers") (filter (not . null) $ (.reproducer) <$> tests)

      saveTracesEnabled <- lookupEnv "ECHIDNA_SAVE_TRACES"
      when (isJust saveTracesEnabled) $ do
        measureIO cfg.solConf.quiet "Saving test reproducers-traces" $ do
          flip runReaderT env $ do
            forM_ tests $ \test ->
              unless (null test.reproducer) $ do
                (results, finalVM) <- reproduceTest vm test
                let subdir = dir </> "reproducers-traces"
                liftIO $ createDirectoryIfMissing True subdir
                let file = subdir </> (show . abs . hash . show) test.reproducer <.> "txt"
                txsPrinted <- ppFailWithTraces Nothing finalVM results
                liftIO $ writeFile file (ppTestName test <> ": " <> txsPrinted)

      measureIO cfg.solConf.quiet "Saving corpus" $ do
        corpus <- readIORef env.corpusRef
        saveTxs (dir </> "coverage") (snd <$> Set.toList corpus)

      -- TODO: We use the corpus dir to save coverage reports which is confusing.
      -- Add config option to pass dir for saving coverage report and decouple it
      -- from corpusDir.
      unless (null cfg.campaignConf.coverageFormats) $ measureIO cfg.solConf.quiet "Saving coverage" $ do
        -- We need runId to have a unique directory to save files under so they
        -- don't collide with the next runs. We use the current time for this
        -- as it orders the runs chronologically.
        runId <- fromIntegral . systemSeconds <$> getSystemTime

        Onchain.saveCoverageReport env runId

        -- save source coverage reports
        let contracts = Map.elems env.dapp.solcByName
        saveCoverages env runId dir buildOutput.sources contracts

  if isSuccessful tests then exitSuccess else exitWith (ExitFailure 1)

data Options = Options
  { cliFilePath         :: NE.NonEmpty FilePath
  , cliWorkers          :: Maybe Word8
  , cliServerPort       :: Maybe Word16
  , cliSelectedContract :: Maybe Text
  , cliConfigFilepath   :: Maybe FilePath
  , cliOutputFormat     :: Maybe OutputFormat
  , cliCorpusDir        :: Maybe FilePath
  , cliTestMode         :: Maybe TestMode
  , cliAllContracts     :: Bool
  , cliTimeout          :: Maybe Int
  , cliTestLimit        :: Maybe Int
  , cliRpcBlock         :: Maybe Word64
  , cliRpcUrl           :: Maybe Text
  , cliShrinkLimit      :: Maybe Int
  , cliSeqLen           :: Maybe Int
  , cliContractAddr     :: Maybe Addr
  , cliDeployer         :: Maybe Addr
  , cliSender           :: [Addr]
  , cliSeed             :: Maybe Int
  , cliCryticArgs       :: Maybe String
  , cliSolcArgs         :: Maybe String
  , cliSymExec          :: Maybe Bool
  , cliSymExecTimeout   :: Maybe Int
  , cliSymExecNSolvers  :: Maybe Int
  }

optsParser :: ParserInfo Options
optsParser = info (helper <*> versionOption <*> options) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

bool :: ReadM Bool
bool = maybeReader (f . map toLower) where
  f "true" = Just True
  f "false" = Just False
  f _ = Nothing

options :: Parser Options
options = Options
  <$> (NE.fromList <$> some (argument str (metavar "FILES"
    <> help "Solidity files to analyze")))
  <*> optional (option auto $ long "workers"
    <> metavar "N"
    <> help "Number of workers to run")
  <*> optional (option auto $ long "server"
    <> metavar "PORT"
    <> help "Run events server on the given port")
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
  <*> optional (option auto $ long "timeout"
    <> metavar "INTEGER"
    <> help "Timeout given in seconds.")
  <*> optional (option auto $ long "test-limit"
    <> metavar "INTEGER"
    <> help ("Number of sequences of transactions to generate during testing. Default is " ++ show defaultTestLimit))
  <*> optional (option auto $ long "rpc-block"
    <> metavar "BLOCK"
    <> help "Block number to use when fetching over RPC.")
  <*> optional (option str $ long "rpc-url"
    <> metavar "URL"
    <> help "RPC URL to fetch contracts over.")
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
  <*> optional (option bool $ long "sym-exec"
    <> metavar "BOOL"
    <> help "Whether to enable the experimental symbolic execution feature.")
  <*> optional (option auto $ long "sym-exec-timeout"
    <> metavar "INTEGER"
    <> help ("Timeout for each symbolic execution run, in seconds (assuming sym-exec is enabled). Default is " ++ show defaultSymExecTimeout))
  <*> optional (option auto $ long "sym-exec-n-solvers"
    <> metavar "INTEGER"
    <> help ("Number of symbolic execution solvers to run in parallel for each task (assuming sym-exec is enabled). Default is " ++ show defaultSymExecNWorkers))

versionOption :: Parser (a -> a)
versionOption = infoOption
                  ("Echidna " ++ showVersion version)
                  (long "version" <> help "Show version")

overrideConfig :: EConfig -> Options -> IO EConfig
overrideConfig config Options{..} = do
  envRpcUrl <- Onchain.rpcUrlEnv
  envRpcBlock <- Onchain.rpcBlockEnv
  pure $
    config { solConf = overrideSolConf config.solConf
           , campaignConf = overrideCampaignConf config.campaignConf
           , uiConf = overrideUiConf config.uiConf
           , rpcUrl = cliRpcUrl <|> envRpcUrl <|> config.rpcUrl
           , rpcBlock = cliRpcBlock <|> envRpcBlock <|> config.rpcBlock
           }
           & overrideFormat
  where
    overrideUiConf uiConf = uiConf
      { maxTime = cliTimeout <|> uiConf.maxTime
      }

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
      , workers = cliWorkers <|> campaignConf.workers
      , serverPort = cliServerPort <|> campaignConf.serverPort
      , symExec = fromMaybe campaignConf.symExec cliSymExec
      , symExecTimeout = fromMaybe campaignConf.symExecTimeout cliSymExecTimeout
      , symExecNSolvers = fromMaybe campaignConf.symExecNSolvers cliSymExecNSolvers
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
