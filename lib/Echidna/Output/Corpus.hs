module Echidna.Output.Corpus where

import Control.Concurrent (forkFinally, Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad (unless, void, when)
import Control.Monad.Extra (unlessM)
import Data.Aeson (ToJSON(..), decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Hashable (hash)
import Data.Maybe (catMaybes, fromMaybe)
import System.Directory (createDirectoryIfMissing, makeRelativeToCurrentDirectory, doesFileExist)
import System.FilePath ((</>), (<.>))

import Echidna.Types.Campaign (CampaignEvent(..), CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Test (EchidnaTest(..))
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

-- save to corpus in the background while tests are running
-- returns a channel that gets sent () when the process is done
runCorpusSaver :: Env -> IO (Chan ())
runCorpusSaver env = do
  finishChan <- newChan
  case env.cfg.campaignConf.corpusDir of
    Nothing -> writeChan finishChan ()
    Just dir -> do
      -- we want to dupChan *before* forking so we don't miss any events
      chan <- dupChan env.eventQueue
      void $ forkFinally (loop dir chan nworkers) (const $ writeChan finishChan ())
  pure finishChan
  where
    nworkers :: Int
    nworkers = fromIntegral $ fromMaybe 1 env.cfg.campaignConf.workers

    loop !dir !chan !workersAlive = when (workersAlive > 0) $ do
      (_, _, event) <- readChan chan
      saveEvent dir event
      case event of
        WorkerStopped _ -> loop dir chan (workersAlive - 1)
        _               -> loop dir chan workersAlive

    saveEvent dir (TestFalsified test) = saveFile dir "reproducers" test.reproducer
    saveEvent dir (TestOptimized test) = saveFile dir "reproducers" test.reproducer
    saveEvent dir (NewCoverage _ _ _ txs) = saveFile dir "coverage" txs
    saveEvent _ _ = pure ()

    saveFile dir subdir txs = unless (null txs) $ saveTxs (dir </> subdir) [txs]
