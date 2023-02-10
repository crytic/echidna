{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Lens (view)
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandomR)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey)
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Types (toJSONKeyText)
import Data.ByteString qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Vector qualified as Vector
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative
import Paths_echidna (version)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

import EVM (Contract(..), bytecode, ContractCode (RuntimeCode), RuntimeCode (ConcreteRuntimeCode), initialContract)
import EVM.Dapp (dappInfo, emptyDapp)
import EVM.Solidity (SolcContract(..), SourceCache(..))
import EVM.Types (Addr, keccak', W256)

import Echidna
import Echidna.Config
import Echidna.Types.Buffer (forceBuf)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Solidity
import Echidna.Types.Test (TestMode, EchidnaTest(..))
import Echidna.Test (validateTestMode)
import Echidna.Campaign (isSuccessful)
import Echidna.UI
import Echidna.Output.Source
import Echidna.Output.Corpus
import Etherscan qualified

main :: IO ()
main = do
  createDirectoryIfMissing True ".echidna"

  maybeBlock :: Maybe Int <- lookupEnv "ECHIDNA_RPC_BLOCK" <&> (>>= readMaybe)

  (loadedContractsCache, loadedSlotsCache) <- case maybeBlock of
    Just block -> do
      parsedContracts :: Maybe (Map Addr FetchedContractData) <-
        readFileIfExists (".echidna" </> "block_" <> show block <> "_fetch_cache_contracts.json")
        <&> (>>= JSON.decodeStrict)

      parsedSlots :: Maybe (Map Addr (Map W256 (Maybe W256))) <-
        readFileIfExists (".echidna" </> "block_" <> show block <> "_fetch_cache_slots.json")
        <&> (>>= JSON.decodeStrict)

      pure (Map.map (Just . fromFetchedContractData) <$> parsedContracts, parsedSlots)
    Nothing ->
      pure (Nothing, Nothing)

  opts@Options{..} <- execParser optsParser
  g <- getRandomR (0, maxBound)
  EConfigWithUsage loadedCfg ks _ <-
    maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig cliConfigFilepath
  let cfg = overrideConfig loadedCfg opts
  unless cfg.solConf.quiet $
    mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . Aeson.Key.toString) ks

  cacheContractsRef <- newIORef $ fromMaybe mempty loadedContractsCache
  cacheSlotsRef <- newIORef $ fromMaybe mempty loadedSlotsCache
  cacheMetaRef <- newIORef mempty
  let env = Env { cfg = cfg
                , dapp = emptyDapp -- TODO: fixme
                , metadataCache = cacheMetaRef
                , fetchContractCache = cacheContractsRef
                , fetchSlotCache = cacheSlotsRef }
  (vm, sourceCache, deployedContracts, world, ts, d, txs) <-
    prepareContract env cliFilePath cliSelectedContract g
  let solcByName = Map.fromList [(c.contractName, c) | c <- deployedContracts]
  -- TODO put in real path
  let dappInfo' = dappInfo "/" solcByName sourceCache
  -- start ui and run tests
  campaign <- runReaderT (ui vm world ts (Just d) txs) (env { dapp = dappInfo' })

  contractsCache <- readIORef cacheContractsRef
  slotsCache <- readIORef cacheSlotsRef

  case maybeBlock of
    Just block -> do
      -- Save fetched data, it's okay to override as the cache only grows
      JSON.encodeFile (".echidna" </> "block_" <> show block <> "_fetch_cache_contracts.json") (toFetchedContractData <$> Map.mapMaybe id contractsCache)
      JSON.encodeFile (".echidna" </> "block_" <> show block <> "_fetch_cache_slots.json") slotsCache
    Nothing ->
      pure ()

  -- save corpus
  case cfg.campaignConf.corpusDir of
    Nothing -> pure ()
    Just dir -> do
      saveTxs (dir </> "reproducers") (filter (not . null) $ (.testReproducer) <$> campaign._tests)
      saveTxs (dir </> "coverage") (snd <$> Set.toList campaign._corpus)

      -- TODO: Add another option to config for saving coverage report

      -- We need runId to have a unique directory to save files under so they
      -- don't collide with the next runs. We use the current time for this
      -- as it orders the runs chronologically.
      runId <- fromIntegral . systemSeconds <$> getSystemTime

      mapM_ (\(addr,mc) ->
        case mc of
          Just contract -> do
            r <- externalSolcContract addr contract
            case r of
              Just (externalSourceCache, solcContract) ->
                let dir' = dir </> show addr
                in do
                  saveCoverage False runId dir' externalSourceCache [solcContract] campaign._coverage
                  saveCoverage True  runId dir' externalSourceCache [solcContract] campaign._coverage
              Nothing -> pure ()
          Nothing -> pure ()) (Map.toList contractsCache)

      -- save source coverage reports
      saveCoverage False runId dir sourceCache deployedContracts campaign._coverage
      saveCoverage True  runId dir sourceCache deployedContracts campaign._coverage

  if isSuccessful campaign then exitSuccess else exitWith (ExitFailure 1)

  where

  -- | "Reverse engineer" the SolcContract and SourceCache structures for the
  -- code fetched from the outside
  externalSolcContract :: Addr -> Contract -> IO (Maybe (SourceCache, SolcContract))
  externalSolcContract addr c = do
    let runtimeCode = forceBuf $ view bytecode c
    putStr $ "Fetching Solidity source for contract at address " <> show addr <> "... "
    -- TODO: without ETHERSCAN_API_KEY there is 1req/5s limit
    srcRet <- Etherscan.fetchContractSource addr
    putStrLn $ if isJust srcRet then "Success!" else "Error!"
    putStr $ "Fetching Solidity source map for contract at address " <> show addr <> "... "
    srcmapRet <- Etherscan.fetchContractSourceMap addr
    putStrLn $ if isJust srcmapRet then "Success!" else "Error!"
    pure $ do
      src <- srcRet
      (_, srcmap) <- srcmapRet
      let sourceCache = SourceCache
            { files = [(Text.pack (show addr), UTF8.fromString src.code)]
            , lines = [(Vector.fromList . BS.split 0xa . UTF8.fromString) src.code]
            , asts = mempty
            }
      let solcContract = SolcContract
            { runtimeCode = runtimeCode
            , creationCode = mempty
            , runtimeCodehash = keccak' runtimeCode
            , creationCodehash = keccak' mempty
            , runtimeSrcmap = mempty
            , creationSrcmap = srcmap
            , contractName = src.name
            , constructorInputs = [] -- error "TODO: mkConstructor abis TODO"
            , abiMap = mempty -- error "TODO: mkAbiMap abis"
            , eventMap = mempty -- error "TODO: mkEventMap abis"
            , errorMap = mempty -- error "TODO: mkErrorMap abis"
            , storageLayout = Nothing
            , immutableReferences = mempty -- TODO: deprecate combined-json
            }
      pure (sourceCache, solcContract)

data FetchedContractData = FetchedContractData
  { runtimeCode :: BS.ByteString
  , nonce :: W256
  , balance :: W256
  }
  deriving (Generic, ToJSON, FromJSON, Show)

fromFetchedContractData :: FetchedContractData -> Contract
fromFetchedContractData contractData =
  (initialContract (EVM.RuntimeCode (EVM.ConcreteRuntimeCode contractData.runtimeCode)))
    { _nonce = contractData.nonce
    , _balance = contractData.balance
    , _external = True
    }

toFetchedContractData :: Contract -> FetchedContractData
toFetchedContractData contract =
  let code = case contract._contractcode of
               RuntimeCode (ConcreteRuntimeCode c) -> c
               _ -> error "unexpected code"
  in FetchedContractData
    { runtimeCode = code
    , nonce = contract._nonce
    , balance = contract._balance
    }

instance ToJSONKey W256 where
  toJSONKey = toJSONKeyText (Text.pack . show)

instance ToJSONKey Addr where
  toJSONKey = toJSONKeyText (Text.pack . show)

readFileIfExists :: FilePath -> IO (Maybe BS.ByteString)
readFileIfExists path = do
  exists <- doesFileExist path
  if exists then Just <$> BS.readFile path else pure Nothing

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
