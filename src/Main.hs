{-# LANGUAGE OverloadedStrings, TupleSections  #-}

module Main where

import Control.Lens hiding (argument)
import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad           (forM, replicateM_)
import Control.Monad.Reader    (runReader)
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
  (Options file contract configFile) <- execParser opts
  config <- maybe (pure defaultConfig) parseConfig configFile

  -- Load solidity contract and get VM
  (v,a,ts) <- loadSolidity file (pack <$> contract) (pack <$> (config ^. solcArgs))

  if (config ^. epochs) <= 0
    -- Run without coverage
    then do
    let prop t = (PropertyName $ show t
                 , runReader (ePropertySeq (flip checkETest t) a v) config
                 )

    _ <- checkParallel . Group (GroupName file) $ map prop ts
    return ()

    -- Run with coverage
    else do
    tests <- mapM (\t -> fmap (t,) (newMVar [])) ts
    let prop (cov,t,mvar) = (PropertyName $ show t
                          , runReader (ePropertySeqCoverage cov mvar (flip checkETest t) a v) config
                          )

    replicateM_ (config ^. epochs) $ do
      xs <- forM tests $ \(x,y) -> do
        cov <- readMVar y
        lastGen <- getCover cov
        _ <- swapMVar y []
        return (lastGen,x,y)

      checkParallel . Group (GroupName file) $ map prop xs
        
    ls <- mapM (readMVar . snd) tests
    let l = size $ foldl' (\acc xs -> unions (acc : map snd xs)) mempty ls
    putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
