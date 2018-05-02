{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar (takeMVar, newMVar)
import Data.MultiSet           (distinctSize)
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
  , solcArgs         :: Maybe String }

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

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Fuzzing/property based testing of EVM code"
  <> header "Echidna - Ethereum fuzz testing framework" )

main :: IO ()
main = do
  (Options f c s) <- execParser opts
  (v,a,ts) <- loadSolidity f (pack <$> c) (pack <$> s)
  r        <- newMVar (mempty :: Coverage)
  let prop t = (PropertyName $ show t
               , ePropertySeqCoverage r (flip checkETest t) a v 10
               )
  _ <- checkParallel . Group (GroupName f) $ map prop ts
  l <- distinctSize <$> takeMVar r
  putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
  return ()
