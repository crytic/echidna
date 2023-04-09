module Echidna.Output.Corpus where

import Control.Monad.Extra (unlessM)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)
import System.FilePath ((</>), (<.>))

import Echidna.Types.Tx (Tx)
import Echidna.Utility (listDirectory, withCurrentDirectory)

saveTxs :: FilePath -> [[Tx]] -> IO ()
saveTxs dir = mapM_ saveTxSeq where
  saveTxSeq txSeq = do
    let file = dir </> (show . hash . show) txSeq <.> "txt"
    unlessM (doesFileExist file) $ encodeFile file (toJSON txSeq)

loadTxs :: FilePath -> IO [[Tx]]
loadTxs dir = do
  createDirectoryIfMissing True dir
  files <- listDirectory dir
  css <- mapM readCall <$> mapM makeRelativeToCurrentDirectory files
  txSeqs <- catMaybes <$> withCurrentDirectory dir css
  putStrLn ("Loaded " ++ show (length txSeqs) ++ " transaction sequences from " ++ dir)
  pure txSeqs
  where readCall f = decodeStrict <$> BS.readFile f
