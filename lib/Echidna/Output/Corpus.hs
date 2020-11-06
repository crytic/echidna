module Echidna.Output.Corpus where

import Prelude hiding (Word)

import Control.Monad (unless)
import Control.Monad.Catch (bracket)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing)

import qualified System.Directory as SD
import qualified Data.ByteString as BS

import Echidna.Types.Tx

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> SD.getDirectoryContents path
  where f filename = filename /= "." && filename /= ".."

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket SD.getCurrentDirectory SD.setCurrentDirectory $ \_ -> do
    SD.setCurrentDirectory dir
    action

saveTxs :: Maybe FilePath -> [[Tx]] -> IO ()
saveTxs (Just d) txs = mapM_ saveTx txs where
  saveTx v = do let fn = d ++ "/coverage/" ++ (show . hash . show) v ++ ".txt"
                b <- SD.doesFileExist fn
                unless b $ encodeFile fn (toJSON v)
saveTxs Nothing  _   = pure ()

loadTxs :: Maybe FilePath -> IO [[Tx]]
loadTxs (Just d) = do
  let d' = d ++ "/coverage"
  createDirectoryIfMissing True d' 
  fs <- listDirectory d'
  css <- mapM readCall <$> mapM SD.makeRelativeToCurrentDirectory fs
  txs <- catMaybes <$> withCurrentDirectory d' css
  putStrLn ("Loaded total of " ++ show (length txs) ++ " transactions from " ++ d')
  return txs
  where readCall f = decodeStrict <$> BS.readFile f

loadTxs Nothing  = pure []
