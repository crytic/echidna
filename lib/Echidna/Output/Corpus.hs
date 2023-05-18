module Echidna.Output.Corpus where

import Control.Exception (handle, IOException)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Extra (unlessM)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)
import System.FilePath ((</>), (<.>))

import Echidna.Async (addEventHandler, pushEventIO)
import Echidna.Types.Campaign (CampaignEvent(..), CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Test (EchidnaTest(..))
import Echidna.Types.Tx (Tx)
import Echidna.Utility (listDirectory, withCurrentDirectory)

saveTxs :: FilePath -> String -> [[Tx]] -> IO ()
saveTxs dir prefix = mapM_ saveTxSeq where
  saveTxSeq txSeq = do
    let file = dir </> prefix ++ (show . abs . hash . show) txSeq <.> "txt"
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

-- setup a handler to save to corpus in the background while tests are running
setupCorpusSaver :: (MonadReader Env m, MonadIO m) => m ()
setupCorpusSaver = do
  env <- ask
  maybe (pure ()) (addEventHandler . saveEvent env) env.cfg.campaignConf.corpusDir
  where
    saveEvent env dir (workerId, _, event) = maybe (pure ()) (saveFile workerId env dir) $ getEventInfo event

    getEventInfo (TestFalsified test) = Just ("reproducers", "unshrunk-", test.reproducer)
    getEventInfo (TestOptimized test) = Just ("reproducers", "", test.reproducer)
    getEventInfo (TestShrunk    test) = Just ("reproducers", "", test.reproducer)
    getEventInfo (NewCoverage _ _ _ txs) = Just ("coverage", "", txs)
    getEventInfo _ = Nothing

    saveFile workerId env dir (subdir, prefix, txs) = unless (null txs) $ handle (exceptionHandler workerId env) $ saveTxs (dir </> subdir) prefix [txs]

    exceptionHandler workerId env (e :: IOException) = pushEventIO env workerId . HandlerFailed $ "Problem while writing to file: " ++ show e
