module Echidna.Output.Corpus where

import Control.Exception (IOException, handle)
import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Hashable (hash)
import Data.Maybe (catMaybes)
import Data.Time (LocalTime)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)
import System.FilePath ((</>), (<.>))

import Echidna.Campaign (pushCampaignEvent)
import Echidna.Types.Config
import Echidna.Types.Campaign
import Echidna.Types.Test (EchidnaTest(..))
import Echidna.Types.Tx (Tx)
import Echidna.Utility (listDirectory, withCurrentDirectory)

saveTxs :: FilePath -> [[Tx]] -> IO ()
saveTxs dir = mapM_ saveTxSeq where
  saveTxSeq txSeq = do
    createDirectoryIfMissing True dir
    let file = dir </> (show . abs . hash . show) txSeq <.> "txt"
    unlessM (doesFileExist file) $ encodeFile file (toJSON txSeq)

loadTxs :: FilePath -> IO [(FilePath, [Tx])]
loadTxs dir = do
  createDirectoryIfMissing True dir
  files <- listDirectory dir
  css <- mapM (\file -> fmap (file,) <$> readCall file) <$> mapM makeRelativeToCurrentDirectory files
  txSeqs <- catMaybes <$> withCurrentDirectory dir css
  putStrLn ("Loaded " ++ show (length txSeqs) ++ " transaction sequences from " ++ dir)
  pure txSeqs
  where readCall f = decodeStrict <$> BS.readFile f

-- Save corpus/reproducers transactions based on an event
saveCorpusEvent :: Env -> (LocalTime, CampaignEvent) -> IO ()
saveCorpusEvent env (_time, campaignEvent) = do
  case env.cfg.campaignConf.corpusDir of
    Just corpusDir -> saveEvent corpusDir campaignEvent
    Nothing -> pure ()
  where
    saveEvent dir (WorkerEvent _workerId _workerType event) =
      maybe (pure ()) (saveFile dir) $ getEventInfo event
    saveEvent _ _ = pure ()

    getEventInfo = \case
      -- TODO: We save intermediate reproducers in separate directories.
      -- This is to because there can be a lot of them and we want to skip
      -- loading those on startup. Ideally, we should override the same file
      -- with a better version of a reproducer, this is smaller or more optimized.
      TestFalsified test -> Just ("reproducers-unshrunk", test.reproducer)
      TestOptimized test -> Just ("reproducers-optimizations", test.reproducer)
      NewCoverage { transactions } -> Just ("coverage", transactions)
      _ -> Nothing

    saveFile dir (subdir, txs) =
      unless (null txs) $
        handle exceptionHandler $ saveTxs (dir </> subdir) [txs]

    exceptionHandler (e :: IOException) =
      pushCampaignEvent env (Failure $ "Problem while writing to file: " ++ show e)
