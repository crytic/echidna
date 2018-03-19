{-# LANGUAGE LambdaCase #-}

module Main where

import Data.IORef         (newIORef, readIORef)
import System.Environment (getArgs)
import Data.Maybe         (listToMaybe)
import Data.Text          (pack)

import Echidna.Exec
import Echidna.Solidity

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  filepath:args -> do
    (v,a,ts) <- loadSolidity filepath $ pack <$> listToMaybe args
    r        <- newIORef (mempty :: Coverage)
    let prop t = ( PropertyName $ show t
                 , ePropertySeqCoverage r (const $ flip checkETest t) a v 10
                 )
    _ <- checkParallel . Group (GroupName filepath) $ map prop ts
    l <- length <$> readIORef r
    putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
    return ()
