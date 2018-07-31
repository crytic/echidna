{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (Word)

import Control.Lens (view)
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execState)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Text (unpack)
import EVM (gas, state)
import Hedgehog.Gen (sample, prune)
import System.Environment (getArgs)

import Echidna.ABI
import Echidna.Config
import Echidna.Exec
import Echidna.Solidity


main :: IO ()
main = let conf m = runReaderT m defaultConfig in getArgs >>= \case
  []     -> putStrLn "need to specify solidity file"
  prog:_ -> do
    (v,a,_) <- conf $ loadSolidity prog Nothing
    forM_ a $ \f -> do
      inputs <- liftIO $ replicateM 1000 . sample . prune . conf $ genAbiCall f
      let results = map (\i -> (i, g v - g (execState (execCall i) v))) inputs; g = view $ state . gas
      mapM_ putStrLn [ unpack $ fst f
                     , pp maximumBy "Max" results, pp minimumBy "Min" results
                     , "  Avg: " ++ show ( fromIntegral (sum $ snd <$> results)
                                         / fromIntegral (length results) :: Double)
                     ] where pp f s r = let (c, n) = f (comparing snd) r in
                                            "  " ++ s ++ ": " ++ show (toInteger n)
                                                 ++ " (" ++ displayAbiCall c ++ ")"
