{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (forM_, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execState)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Text (unpack)
import EVM (gas, state)
import Hedgehog.Gen (sample)
import System.Environment (getArgs)

import Echidna.ABI
import Echidna.Config
import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = let conf m = runReaderT m defaultConfig in getArgs >>= \case
  []     -> putStrLn "need to specify solidity file"
  prog:_ -> do
    (v,a,_) <- conf (loadSolidity prog Nothing)
    forM_ a $ \f -> do
      inputs <- liftIO $ replicateM 10000 . sample . conf $ genInteractions [f]
      let results = map (\i -> (i, (v ^. state . gas) - (execState (execCall i) v ^. state . gas))) inputs
      mapM_ putStrLn [ unpack $ fst f
                     , let m = maximumBy (comparing snd) results in
                       "  Max: " ++ show (toInteger $ snd m) ++ " (" ++ displayAbiCall (fst m) ++ ")"
                     , let m = minimumBy (comparing snd) results in
                       "  Min: " ++ show (toInteger $ snd m) ++ " (" ++ displayAbiCall (fst m) ++ ")"
                     , "  Avg: " ++ show (( fromIntegral $ sum (snd <$> results))
                                          / fromIntegral (length results) :: Double)
                     ]
