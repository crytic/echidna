{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forM_)
import Hedgehog (check)
import System.Environment (getArgs)

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  f:_ -> loadSolidity f >>= \(v,a,ts) -> forM_ ts $ \t -> do
    putStrLn ("[*] Checking test " ++ show t)
    check $ ePropertySeq v a (`checkETest` t) 10
