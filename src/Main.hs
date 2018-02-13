{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import System.Environment

import EVM.ABI
import EVM

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  f:_ -> do (v,a,t) <- loadSolidity f
            mapM_ (test v a) t

test :: VM -> [(Text, [AbiType])] -> Text -> IO ()
test v a t = do print ("Checking test " <> t <> "...")
                res <- fuzz 10 10000 a v (\x -> return $ checkETest x t) 
                putStrLn $ maybe "Test passed!"
                                 (("Test failed! Counterexample: " ++) . show) res
