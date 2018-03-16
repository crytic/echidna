{-# LANGUAGE LambdaCase #-}

module Main where

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))
import System.Environment         (getArgs)
import Data.Maybe (listToMaybe)
import Data.Text (pack)

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  filepath:args -> do
    (v,a,ts) <- loadSolidity filepath $ pack <$> listToMaybe args
    let prop t = (PropertyName $ show t, ePropertySeq v a (`checkETest` t) 10)
    _ <- checkParallel . Group (GroupName filepath) $ map prop ts
    return ()
