module Echidna.Output.Corpus where

import Prelude hiding (Word)

import Control.Monad (unless)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)

import qualified Data.ByteString as BS

import Echidna.Types.Tx
import Echidna.Output.Utils

saveTxs :: Maybe FilePath -> [(Integer, Integer, [Tx])] -> IO ()
saveTxs (Just d) cs = mapM_ saveTx cs where
  saveTx (_,i,v) = do let fn = d ++ "/coverage/" ++ (show . hash . show) v ++ ".txt." ++ show i
                      b <- doesFileExist fn
                      unless b $ encodeFile fn (toJSON v)
saveTxs Nothing  _   = pure ()

loadTxs :: Maybe FilePath -> IO [[Tx]]
loadTxs (Just d) = do
  let d' = d ++ "/coverage"
  createDirectoryIfMissing True d' 
  fs <- listDirectory d'
  css <- mapM readCall <$> mapM makeRelativeToCurrentDirectory fs
  txs <- catMaybes <$> withCurrentDirectory d' css
  putStrLn ("Loaded total of " ++ show (length txs) ++ " transactions from " ++ d')
  return txs
  where readCall f = decodeStrict <$> BS.readFile f

loadTxs Nothing  = pure []
