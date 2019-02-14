module Main where

import Control.Lens hiding (argument)
import Control.Monad.Reader (runReaderT)
import Data.Text (pack)
import Options.Applicative
import EVM
import EVM.ABI 

import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.Transaction
import Echidna.UI
import Echidna.ABI

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

enableCoverage :: EConfig -> EConfig
enableCoverage x = x & cConf %~ (\k -> k {knownCoverage = Just mempty})  

enableDict :: EConfig -> [AbiValue] -> EConfig
enableDict x y = x & gConf %~ const (mkConf 0.25 y [])

main :: IO ()
main = do (Options f c cov conf) <- execParser opts
          cfg <- maybe (pure defaultConfig) parseConfig conf
          let cfg' = if cov then enableCoverage cfg else cfg 
          (v,a,ts,vs) <- loadSolidity (cfg ^. sConf) f (pack <$> c)
          let cfg'' = enableDict cfg' vs
          flip runReaderT cfg'' $ do
            let r = v ^. state . contract
            let w = World (cfg ^. sConf . sender) [(r, a)]
            let ts' = zip ts $ repeat r
            ui v w ts' >> pure ()
