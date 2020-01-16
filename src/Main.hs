module Main where

import Control.Lens (view)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Data.Text (pack)
import Data.Version (showVersion)
import Options.Applicative
import Paths_echidna (version)
import System.Exit (exitWith, exitSuccess, ExitCode(..))

import Echidna.ABI
import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.UI
import Echidna.Transaction

data Options = Options
  { filePath         :: FilePath
  , selectedContract :: Maybe String
  , configFilepath   :: Maybe FilePath
  }

options :: Parser Options
options = Options <$> argument str (metavar "FILE"
                        <> help "Solidity file to analyze")
                  <*> optional (argument str $ metavar "CONTRACT"
                        <> help "Contract to analyze")
                  <*> optional (option str $ long "config"
                        <> help "Config file")

versionOption :: Parser (a -> a)
versionOption = infoOption
                  ("Echidna " ++ showVersion version)
                  (long "version" <> help "Show version")

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> options) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

main :: IO ()
main = do Options f c conf <- execParser opts
          g   <- getRandom
          cfg <- maybe (pure defaultConfig) parseConfig conf
          txs <- loadTrans (corpusDir $ view cConf cfg)
          --print txs
          cpg <- flip runReaderT cfg $ do
            cs       <- contracts f
            ads      <- addresses
            (v,w,ts) <- loadSpecified (pack <$> c) cs >>= prepareForTest
            ui v w ts (Just $ mkGenDict 0.50 (extractConstants cs ++ ads) [] g (returnTypes cs)) txs
          saveTrans (corpusDir $ view cConf cfg) (view genTrans cpg)
          if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess
