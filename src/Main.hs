{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent      (forkIO)
import Control.Concurrent.MVar (takeMVar, newMVar)
import Control.Concurrent.Chan (newChan)
import Data.Maybe              (listToMaybe)
import Data.Set                (size)
--import Data.MultiSet           (distinctSize)
import Data.Text               (pack)
import System.Environment      (getArgs)

import Echidna.Exec
import Echidna.Solidity

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  filepath:args -> do
    (v,a,ts)   <- loadSolidity filepath $ pack <$> listToMaybe args
    execEnvRef <- newMVar $ ExecEnv {coverage = mempty, recent = [0,0,0,0,0,0], avg = 0}
    goodInputs <- newChan
    _          <- forkIO $ mutateGoodInputs goodInputs
    let prop t = (PropertyName $ show t
                 , ePropertySeqCoverage execEnvRef goodInputs (flip checkETest t) a v 10
                 )
    _ <- checkParallel . Group (GroupName filepath) $ map prop ts
    l <- size . coverage <$> takeMVar execEnvRef
    putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
    return ()
