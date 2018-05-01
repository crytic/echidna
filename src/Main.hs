{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Monad           (forM_, forM)
import Data.Maybe              (listToMaybe)
import Data.Set                (size, unions)
import Data.Text               (pack)
import System.Environment      (getArgs)

import Echidna.Exec
import Echidna.Solidity

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"

  --RUN USING COVERAGE GUIDANCE
  filepath:n:args -> do
    (v,a,ts)   <- loadSolidity filepath $ pack <$> listToMaybe args
    tests      <- mapM (\t -> return . ((,) t) =<< newMVar []) ts
    let prop (cov,t,mvar) = (PropertyName $ show t
                            , ePropertySeqCoverage cov mvar (flip checkETest t) a v 10
                            )

    let epochs = read n :: Int
    forM_ [1..epochs] $ \i -> do
      putStrLn $ "Beginning Epoch " ++ show i
      xs <- forM tests $ \(x,y) -> do
        cov <- readMVar y
        lastGen <- getCover cov
        modifyMVar_ y (\_ -> pure [])
        return (lastGen,x,y)

      checkParallel . Group (GroupName filepath) $ map prop xs

    ls <- mapM (readMVar . snd) tests
    let l = size $ foldl (\acc xs -> unions (acc:(map snd xs))) mempty ls
    putStrLn $ "Coverage: " ++ show l ++ " unique PCs"
    return ()

  --RUN WITHOUT COVERAGE GUIDANCE
  filepath:args -> do
    (v,a,ts) <- loadSolidity filepath $ pack <$> listToMaybe args

    let prop t = (PropertyName $ show t
                 , ePropertySeq (flip checkETest t) a v 10
                 )

    _ <- checkParallel . Group (GroupName filepath) $ map prop ts
    return ()
