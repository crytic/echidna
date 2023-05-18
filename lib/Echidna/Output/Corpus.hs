module Echidna.Output.Corpus where

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

import Echidna.Async (addEventHandler, spawnThread)
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

-- setup a handler to save to corpus in the background while tests are running
setupCorpusSaver :: (MonadReader Env m, MonadIO m) => m ()
setupCorpusSaver = do
  env <- ask
  maybe (pure ()) (addEventHandler . saveEvent env) env.cfg.campaignConf.corpusDir
  where
    saveEvent env dir (_, _, TestFalsified test) = saveFile env dir "reproducers" test.reproducer
    saveEvent env dir (_, _, TestOptimized test) = saveFile env dir "reproducers" test.reproducer
    saveEvent env dir (_, _, TestSimplified test) = saveFile env dir "reproducers" test.reproducer
    saveEvent env dir (_, _, NewCoverage _ _ _ txs) = saveFile env dir "coverage" txs
    saveEvent _ _ _ = pure ()

    saveFile env dir subdir txs = unless (null txs) $ spawnThread env $ saveTxs (dir </> subdir) [txs]
