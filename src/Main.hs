module Main where

import Control.Lens ((^.), to, (.~), (&))
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
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
import Echidna.Types.Campaign
import Echidna.Campaign (isSuccess)
import Echidna.UI
import Echidna.Transaction

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
  opts@(Options f c conf _) <- execParser optsParser
  g <- getRandom
  EConfigWithUsage loadedCfg ks _ <- maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig conf
  let cfg = overrideConfig loadedCfg opts
  unless (cfg ^. sConf . quiet) $ mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . unpack) ks
  let cd = cfg ^. cConf . corpusDir
      df = cfg ^. cConf . dictFreq
  txs <- loadTxs cd
  cpg <- flip runReaderT cfg $ do
    cs       <- Echidna.Solidity.contracts f
    ads      <- addresses
    (v,w,ts) <- loadSpecified (pack <$> c) cs >>= prepareForTest
    let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
    ui v w ts (Just $ mkGenDict df (extractConstants cs ++ NE.toList ads ++ ads') [] g (returnTypes cs)) txs
  saveTxs cd (snd <$> DS.toList (cpg ^. corpus))
  if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess
  where overrideConfig cfg (Options _ _ _ fmt) =
          case maybe (cfg ^. uConf . operationMode) NonInteractive fmt of
               Interactive -> cfg
               NonInteractive Text -> cfg & uConf . operationMode .~ NonInteractive Text
               nonInteractive -> cfg & uConf . operationMode .~ nonInteractive
                                     & sConf . quiet .~ True
