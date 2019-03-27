module Main where

import Control.Lens hiding (argument)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (isJust)
import Data.Text (pack)
import Options.Applicative
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.Random (getStdGen)

import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.UI

data Options = Options
  { filePath         :: FilePath
  , selectedContract :: Maybe String
  , coverageSelector :: Bool
  , configFilepath   :: Maybe FilePath
  }

options :: Parser Options
options = Options <$> argument str (metavar "FILE"
                        <> help "Solidity file to analyze")
                  <*> optional (argument str $ metavar "CONTRACT"
                        <> help "Contract to analyze")
                  <*> switch (long "coverage"
                        <> help "Turn on coverage")
                  <*> optional (option str $ long "config"
                        <> help "Config file")

opts :: ParserInfo Options
opts = info (options <**> helper) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

main :: IO ()
main = do (Options f c cov conf) <- execParser opts
          g <- getStdGen
          cfg          <- maybe (pure defaultConfig) parseConfig conf
          Campaign r _ <- runReaderT (loadSolTests f (pack <$> c) >>= (\(v,w,ts) -> ui v w ts)) $
                            cfg & cConf %~ (if cov then \k -> k {knownCoverage = Just mempty} else id)
                                & cConf %~ \x -> if isJust (seed x) then x else x { seed = Just g }
          if any (/= Passed) $ snd <$> r then exitWith $ ExitFailure 1
                                         else exitSuccess
