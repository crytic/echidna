module Main where

import Control.Lens ((^.), (.~), (&))
import Control.Monad (unless)
import Control.Monad.Reader (runReaderT, when)
import Control.Monad.Random (getRandom)
import Data.Maybe (isJust, fromJust)
import Data.Text (unpack)
import Data.Version (showVersion)
import Options.Applicative
import Paths_echidna (version)
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Directory (createDirectoryIfMissing)

import Echidna
import Echidna.Config
import Echidna.Solidity
import Echidna.Types.Campaign
import Echidna.Campaign (isSuccess)
import Echidna.UI
import Echidna.Transaction
import Echidna.Output.Source

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

  when (isJust cd) $ createDirectoryIfMissing True (fromJust cd <> "/coverage")

  (sc, cs, cpg) <- flip runReaderT cfg $ do
    (v, sc, cs, w, ts, d, txs) <- prepareContract cfg f c g
    -- start ui and run tests
    r <- ui v w ts d txs
    return (sc, cs, r)

  -- save corpus
  saveTxs cd (snd <$> DS.toList (cpg ^. corpus))

  -- save source coverage
  saveCoveredCode cd sc cs (cpg ^. coverage) 

  if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess
  where overrideConfig cfg (Options _ _ _ fmt) =
          case maybe (cfg ^. uConf . operationMode) NonInteractive fmt of
               Interactive -> cfg
               NonInteractive Text -> cfg & uConf . operationMode .~ NonInteractive Text
               nonInteractive -> cfg & uConf . operationMode .~ nonInteractive
                                     & sConf . quiet .~ True
