module Main where

import Control.Monad.Reader (runReaderT)
import Control.Lens hiding (argument)
import Data.Maybe (isJust)
import Data.Text (pack)
import Options.Applicative
import System.Exit (exitWith, exitSuccess, ExitCode(..))
import System.Random (getStdGen)

import Echidna.ABI
import Echidna.Config
import Echidna.Solidity
import Echidna.Campaign
import Echidna.UI

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

opts :: ParserInfo Options
opts = info (options <**> helper) $ fullDesc
  <> progDesc "EVM property-based testing framework"
  <> header "Echidna"

main :: IO ()

{- <<<<<<< HEAD
main = do (Options f c cov conf) <- execParser opts
          g <- getStdGen
          cfg          <- maybe (pure defaultConfig) parseConfig conf
          Campaign r _ <- runReaderT (loadSolTests f (pack <$> c) >>= (\(v,w,ts) -> ui v w ts)) $
                            cfg & cConf %~ (if cov then \k -> k {knownCoverage = Just mempty} else id)
                                & cConf %~ \x -> if isJust (seed x) then x else x { seed = Just g }
          if any (/= Passed) $ snd <$> r then exitWith $ ExitFailure 1
                                         else exitSuccess
======= -}
main = do Options f c conf <- execParser opts
          g    <- getStdGen
          cfg  <- maybe (pure defaultConfig) parseConfig conf
          let cfg' = cfg & cConf %~ \x -> if isJust (seed x) then x else x { seed = Just g }
          cpg  <- flip runReaderT cfg' $ do
            cs       <- contracts f
            (v,w,ts) <- loadSpecified (pack . (f ++) . (':' :) <$> c) cs >>= prepareForTest
            ui v w ts (Just $ mkGenDict 0.15 (extractConstants cs) [])
          if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess
-- >>>>>>> 3d095989fa11fe9a2d0760c3e453620ccc1f3a25
