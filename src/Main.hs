{-# LANGUAGE LambdaCase #-}

module Main where

import Hedgehog
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))
import System.Environment         (getArgs)

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  f:_ -> do
    (v,a,ts) <- loadSolidity f
    let prop t = (PropertyName $ show t, ePropertySeq v a (`checkETest` t) 10)
    _ <- checkParallel . Group (GroupName f) $ map prop ts
    return ()
