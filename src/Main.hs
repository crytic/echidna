{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, DoAndIfThenElse  #-}

module Main where

import Control.Lens hiding (argument)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad           (forM, replicateM_)
import Control.Monad.Catch     (MonadThrow(..))
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Reader    (runReaderT)
import Data.List               (foldl')
import Data.Set                (unions, size)
import Data.Text               (pack)
import Data.Semigroup          ((<>))

import Echidna.Config
import Echidna.Coverage (ePropertySeqCoverage, getCover)
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
  (Options file contract usecov configFile) <- execParser opts
  config <- maybe (pure defaultConfig) parseConfig configFile

  let f = checkTest (config ^. returnType)
      checkGroup = if config ^. outputJson 
        then 
            checkParallelJson 
        else 
            checkParallel

  flip runReaderT config $ do
    -- Load solidity contract and get VM
    (v,a,ts) <- loadSolidity file (pack <$> contract)
    if null ts 
        then throwM NoTests
        else pure ()
    if not $ usecov || config ^. printCoverage
      -- Run without coverage
        then do
            let prop t = ePropertySeq (`f` t) a v >>= \x -> return (PropertyName $ show t, x)
            _ <- checkGroup . Group (GroupName file) =<< mapM prop ts
            return ()

      -- Run with coverage
        else do
            tests <- liftIO $ mapM (\t -> fmap (t,) (newMVar [])) ts
            let prop (cov,t,mvar) =
                    ePropertySeqCoverage cov mvar (`f` t) a v >>= \x -> return (PropertyName $ show t, x)

            replicateM_ (config ^. epochs) $ do
                    xs <- liftIO $ forM tests $ \(x,y) -> swapMVar y [] <&> (, x, y) . getCover
                    checkGroup . Group (GroupName file) =<< mapM prop xs
        
            ls <- liftIO $ mapM (readMVar . snd) tests
            let ci = foldl' (\acc xs -> unions (acc : map snd xs)) mempty ls
            liftIO . putStrLn $ "Coverage: " ++ show (size ci) ++ " unique PC's"
