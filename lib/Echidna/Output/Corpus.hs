module Echidna.Output.Corpus where

import Prelude hiding (Word)

import Control.Monad (unless)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)
import System.FilePath ((</>), (<.>))

import Echidna.Types.Tx
import Echidna.Output.Utils

saveTxs :: FilePath -> [[Tx]] -> IO ()
saveTxs d = mapM_ saveTx where
  saveTx v = do let fn = d </> (show . hash . show) v <.> "txt"
                b <- doesFileExist fn
                unless b $ encodeFile fn (toJSON v)

loadTxs :: FilePath -> IO [[Tx]]
loadTxs dir = do
  createDirectoryIfMissing True dir
  fs <- listDirectory dir
  css <- mapM readCall <$> mapM makeRelativeToCurrentDirectory fs
  txs <- catMaybes <$> withCurrentDirectory dir css
  putStrLn ("Loaded total of " ++ show (length txs) ++ " transactions from " ++ dir)
  return txs
  where readCall f = decodeStrict <$> BS.readFile f
