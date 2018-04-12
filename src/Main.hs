{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.MVar (MVar, takeMVar, newMVar)
import Data.Maybe              (listToMaybe)
import Data.MultiSet           (distinctSize)
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
    (v,a,ts) <- loadSolidity filepath $ pack <$> listToMaybe args
    r        <- newMVar (mempty :: Coverage)
    let prop t = (PropertyName $ show t
                 , ePropertySeqCoverage r (flip checkETest t) a v 10
                 )
    _ <- checkParallel . Group (GroupName filepath) $ map prop ts
    l <- distinctSize <$> takeMVar r
    putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
    return ()
