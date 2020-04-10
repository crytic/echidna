{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens (view, (^.), to, (.~), (&))
import Data.Has                   (Has(..))
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT, liftIO)
import Control.Monad.Random (getRandom)
import Data.Map.Strict (keys)
import Data.Text (pack, unpack)
import Data.Version (showVersion)
import Options.Applicative
import Paths_echidna (version)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import EVM (env, contracts)
import EVM.ABI (AbiValue(AbiAddress))

import Echidna.ABI
import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.UI
import Echidna.Transaction
import Echidna.Processor

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as DS

data Options = Options
  { filePath         :: NE.NonEmpty FilePath
  , selectedContract :: Maybe String
  , configFilepath   :: Maybe FilePath
  , outputFormat     :: Maybe OutputFormat
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
  EConfigWithUsage loadedCfg ks _ <- maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig configFilepath
  let cfg = overrideConfig loadedCfg opts
  unless (cfg ^. sConf . quiet) $ mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . unpack) ks
  let cd = corpusDir $ view cConf cfg

  -- load corpus (if any)
  txs <- loadTxs cd
  cpg <- flip runReaderT cfg $ do

    -- compile and load contracts
    cs <- Echidna.Solidity.contracts filePath
    ads <- addresses
    p <- loadSpecified (pack <$> selectedContract) cs

    -- run processors
    ca <- view (hasLens . cryticArgs)
    si <- runSlither (NE.head filePath) ca
    liftIO $ print si
  
    -- load tests
    (v,w,ts) <- prepareForTest p selectedContract si
    let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
    -- start ui and run tests
    ui v w ts (Just $ mkGenDict (dictFreq $ view cConf cfg) (extractConstants cs ++ NE.toList ads ++ ads') [] g (returnTypes cs)) txs

  -- save corpus
  saveTxs cd (map snd $ DS.toList $ view corpus cpg)
  if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess
  where
  overrideConfig cfg Options{..} =
    case maybe (cfg ^. uConf . operationMode) NonInteractive outputFormat of
      Interactive -> cfg
      NonInteractive Text ->
        cfg & uConf . operationMode .~ NonInteractive Text
      nonInteractive ->
        cfg & sConf . quiet .~ True
            & uConf . operationMode .~ nonInteractive
