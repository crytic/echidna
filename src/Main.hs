{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections  #-}

module Main where

import Control.Lens hiding (argument)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad           (forM, replicateM_)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (runReaderT)
import Data.List               (foldl')
import Data.Set                (size, unions)
import Data.Text               (pack)
import Data.Semigroup          ((<>))

import Echidna.Config
import Echidna.Exec
import Echidna.Solidity

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

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
  (Options file contract coverage configFile) <- execParser opts
  config <- maybe (pure defaultConfig) parseConfig configFile

  (flip runReaderT) config $ do
    -- Load solidity contract and get VM
    (v,a,ts) <- loadSolidity file (pack <$> contract)

    if not coverage
      -- Run without coverage
      then do
      let prop t = ePropertySeq (flip checkETest t) a v >>= \x -> return (PropertyName $ show t, x)
      _ <- checkParallel . Group (GroupName file) =<< mapM prop ts
      return ()

      -- Run with coverage
      else do
      tests <- liftIO $ mapM (\t -> fmap (t,) (newMVar [])) ts
      let prop (cov,t,mvar) =
            ePropertySeqCoverage cov mvar (flip checkETest t) a v >>= \x -> return (PropertyName $ show t, x)


      replicateM_ (config ^. epochs) $ do
        xs <- liftIO $ forM tests $ \(x,y) -> do
          cov <- readMVar y
          lastGen <- getCover cov
          _ <- swapMVar y []
          return (lastGen,x,y)

        checkParallel . Group (GroupName file) =<< mapM prop xs
        
      ls <- liftIO $ mapM (readMVar . snd) tests
      let l = size $ foldl' (\acc xs -> unions (acc : map snd xs)) mempty ls
      liftIO $ putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
