module Echidna.Output.Corpus where

import Prelude hiding (Word)

import Control.Monad (unless)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)

import Echidna.Types.Tx
import Echidna.Output.Utils

saveTxs :: Maybe FilePath -> [[Tx]] -> IO ()
saveTxs (Just d) txs = mapM_ saveTx txs where
  saveTx v = do let fn = d ++ (show . hash . show) v ++ ".txt"
                b <- doesFileExist fn
                unless b $ encodeFile fn (toJSON v)
saveTxs Nothing  _   = pure ()

loadTxs :: Maybe FilePath -> IO [[Tx]]
loadTxs (Just d) = do
  createDirectoryIfMissing True d
  fs <- listDirectory d
  css <- mapM readCall <$> mapM makeRelativeToCurrentDirectory fs
  txs <- catMaybes <$> withCurrentDirectory d css
  putStrLn ("Loaded total of " ++ show (length txs) ++ " transactions from " ++ d)
  return txs
  where readCall f = decodeStrict <$> BS.readFile f

loadTxs Nothing  = pure []
