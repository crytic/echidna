module Main where

import Control.Lens hiding (argument)
import Control.Monad.Reader (runReaderT)
import Data.Text (pack)
import Options.Applicative
import EVM

import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.Transaction
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
          cfg <- maybe (pure defaultConfig) parseConfig conf
          flip runReaderT (cfg & cConf %~ (if cov then \k -> k {knownCoverage = Just mempty}
                                                  else id)) $ do
            (v,a,ts) <- loadSolidity f (pack <$> c)
            let r = v ^. state . contract
            let w = World [0] [(r, a)]
            let ts' = zip ts $ repeat r
            ui v w ts' >> pure ()
