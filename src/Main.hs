{-# LANGUAGE LambdaCase, OverloadedStrings, TupleSections #-}

module Main where

import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad           (forM_, forM)
import Data.List               (foldl')
import Data.Set                (size, unions)
import Data.Text               (pack)
import Data.Semigroup          ((<>))

import Echidna.Exec
import Echidna.Solidity

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Options.Applicative


data Options = Options
  { filePath         :: FilePath
  , selectedContract :: Maybe String
  , solcArgs         :: Maybe String
  , covEpochs        :: Maybe Int }

options :: Parser Options
options = Options
      <$> argument str
          ( metavar "FILE"
         <> help "Solidity file to analyze" )
      <*> optional ( argument str
          ( metavar "CONTRACT"
         <> help "Contract inside of file to analyze" ))
      <*> optional ( strOption
          ( long "solc-args"
         <> help "Optional solidity compiler arguments" ))
      <*> optional ( option auto
          ( long "epochs"
         <> help "Optional number of epochs to run coverage guidance" ))

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Fuzzing/property based testing of EVM code"
  <> header "Echidna - Ethereum fuzz testing framework" )


main :: IO ()
main = do
  (Options f c s n) <- execParser opts
  (v,a,ts) <- loadSolidity f (pack <$> c) (pack <$> s)

  case n of
    -- RUN WITHOUT COVERAGE
    Nothing -> do
      let prop t = (PropertyName $ show t
                   , ePropertySeq (flip checkETest t) a v 10
                   )

      _ <- checkParallel . Group (GroupName f) $ map prop ts
      return ()

    -- RUN WITH COVERAGE
    Just epochs -> do
      tests <- mapM (\t -> return . (t,) =<< newMVar []) ts
      let prop (cov,t,mvar) = (PropertyName $ show t
                              , ePropertySeqCoverage cov mvar (flip checkETest t) a v 10
                              )

      forM_ [1..epochs] $ \_ -> do
        xs <- forM tests $ \(x,y) -> do
          cov <- readMVar y
          lastGen <- getCover cov
          _ <- swapMVar y []
          return (lastGen,x,y)

        checkParallel . Group (GroupName f) $ map prop xs
        
      ls <- mapM (readMVar . snd) tests
      let l = size $ foldl' (\acc xs -> unions (acc:(map snd xs))) mempty ls
      putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
      return ()

