{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections  #-}

module Main where

import Control.Lens hiding (argument)
--import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
--import Control.Monad           (forM_, replicateM_)
import Control.Monad.Catch     (MonadThrow(..))
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (runReaderT)
--import Data.List               (foldl')
--import Data.Set                (unions, size)
import Data.Text               (pack)
import Data.Semigroup          ((<>))

import Echidna.Config
import Echidna.Coverage (ePropertySeqCover)
import Echidna.Exec
import Echidna.Solidity

import Options.Applicative


data Options = Options
  { filePath         :: FilePath
  , selectedContract :: Maybe String
  , coverageSelector :: Bool
  , configFilepath   :: Maybe FilePath
  }

options :: Parser Options
options = Options
      <$> argument str
          ( metavar "FILE"
         <> help "Solidity file to analyze" )
      <*> optional ( argument str
          ( metavar "CONTRACT"
         <> help "Contract inside of file to analyze" ))
      <*> switch
          ( long "coverage"
         <> help "Turn on coverage")
      <*> optional ( option str
          ( long "config"
         <> help "Echidna config file" ))

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Fuzzing/property based testing of EVM code"
  <> header "Echidna - Ethereum fuzz testing framework" )

main :: IO ()
main = do
  -- Read cmd line options and load config
  (Options file contract usecov configFile) <- execParser opts
  config <- maybe (pure defaultConfig) parseConfig configFile

  let f = checkTest (config ^. returnType)
  
  flip runReaderT config $ do
    -- Load solidity contract and get VM
    (v,a,ts) <- loadSolidity file (pack <$> contract)
    if null ts then throwM NoTests else pure ()
    if not $ usecov || config ^. printCoverage
      -- Run without coverage
      then liftIO $ ePropertySeq (map (\t -> (t,flip f t)) ts) a v config
      else liftIO $ ePropertySeqCover (map (\t -> (t,flip f t)) ts) a v config

