module Main where

import Control.Lens (view, (^.), to)
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
          EConfigWithUsage cfg ks _ <- maybe (pure (EConfigWithUsage defaultConfig mempty mempty)) parseConfig conf
          cfg' <- process (NE.head f) c cfg
          unless (cfg ^. sConf . quiet) $ mapM_ (hPutStrLn stderr . ("Warning: unused option: " ++) . unpack) ks
          let cd = corpusDir $ view cConf cfg
          txs <- loadTxs cd
          cpg <- flip runReaderT cfg' $ do
            cs       <- Echidna.Solidity.contracts f
            ads      <- addresses
            (v,w,ts) <- loadSpecified (pack <$> c) cs >>= prepareForTest
            let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
            ui v w ts (Just $ mkGenDict (dictFreq $ view cConf cfg) (extractConstants cs ++ NE.toList ads ++ ads') [] g (returnTypes cs)) txs
          saveTxs cd (map fst $ DS.toList $ view corpus cpg)
          if not . isSuccess $ cpg then exitWith $ ExitFailure 1 else exitSuccess
