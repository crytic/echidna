module Echidna.UI where

import Brick
import Brick.BChan
import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader, asks)
import Control.Monad.Random.Strict (MonadRandom)
import Data.ByteString.Lazy qualified as BS
import Data.IORef
import Data.Maybe (fromMaybe)
import Graphics.Vty (Config, Event(..), Key(..), Modifier(..), defaultConfig, inputMap, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO, forkFinally)
import UnliftIO.Timeout (timeout)

import EVM (VM)

import Echidna.ABI
import Echidna.Campaign (campaign)
import Echidna.Output.JSON qualified
import Echidna.Types.Campaign
import Echidna.Types.Test (EchidnaTest)
import Echidna.Types.Tx (Tx)
import Echidna.Types.World (World)
import Echidna.UI.Report
import Echidna.UI.Widgets
import Echidna.Types.Config

data CampaignEvent = CampaignUpdated Campaign | CampaignTimedout Campaign

-- | Set up and run an Echidna 'Campaign' and display interactive UI or
-- print non-interactive output in desired format at the end
ui :: (MonadCatch m, MonadRandom m, MonadReader Env m, MonadUnliftIO m)
   => VM             -- ^ Initial VM state
   -> World          -- ^ Initial world state
   -> [EchidnaTest]  -- ^ Tests to evaluate
   -> Maybe GenDict
   -> [[Tx]]
   -> m Campaign
ui vm world ts d txs = do
  conf <- asks (.cfg)
  let uiConf = conf._uConf
  ref <- liftIO $ newIORef defaultCampaign
  let updateRef = get >>= liftIO . atomicWriteIORef ref
      secToUsec = (* 1000000)
      timeoutUsec = secToUsec $ fromMaybe (-1) uiConf.maxTime
      runCampaign = timeout timeoutUsec (campaign updateRef vm world ts d txs)
  terminalPresent <- isTerminal
  let effectiveMode = case uiConf.operationMode of
        Interactive | not terminalPresent -> NonInteractive Text
        other -> other
  case effectiveMode of
    Interactive -> do
      bc <- liftIO $ newBChan 100
      let updateUI e = readIORef ref >>= writeBChan bc . e
      ticker <- liftIO $ forkIO $ -- run UI update every 100ms
        forever $ threadDelay 100000 >> updateUI CampaignUpdated
      _ <- forkFinally -- run worker
        (void $ runCampaign >>= \case
          Nothing -> liftIO $ updateUI CampaignTimedout
          Just _ -> liftIO $ updateUI CampaignUpdated
        )
        (const $ liftIO $ killThread ticker)
      let vty = mkVty vtyConfig
      initialVty <- liftIO vty
      app <- customMain initialVty vty (Just bc) <$> monitor
      liftIO $ void $ app (defaultCampaign, Uninitialized)
      final <- liftIO $ readIORef ref
      liftIO . putStrLn $ runReader (ppCampaign final) conf
      pure final

    NonInteractive outputFormat -> do
      result <- runCampaign
      (final, timedout) <- case result of
        Nothing -> do
          final <- liftIO $ readIORef ref
          pure (final, True)
        Just final ->
          pure (final, False)
      case outputFormat of
        JSON ->
          liftIO . BS.putStr $ Echidna.Output.JSON.encodeCampaign final
        Text -> do
          liftIO . putStrLn $ runReader (ppCampaign final) conf
          when timedout $ liftIO $ putStrLn "TIMEOUT!"
        None ->
          pure ()
      pure final

vtyConfig :: Config
vtyConfig = defaultConfig { inputMap = (Nothing, "\ESC[6;2~", EvKey KPageDown [MShift]) :
                                       (Nothing, "\ESC[5;2~", EvKey KPageUp [MShift]) :
                                       inputMap defaultConfig
                          }

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: MonadReader Env m => m (App (Campaign, UIState) CampaignEvent ())
monitor = do
  let cs :: EConfig -> (Campaign, UIState) -> Widget ()
      cs s c = runReader (campaignStatus c) s

      se (AppEvent (CampaignUpdated c')) = put (c', Running)
      se (AppEvent (CampaignTimedout c')) = put (c', Timedout)
      se (VtyEvent (EvKey KEsc _))                         = halt
      se (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = halt
      se _                                                 = pure ()
  conf <- asks (.cfg)
  pure $ App (pure . cs conf) neverShowCursor se (pure ()) (const attrs)

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: MonadIO m => m Bool
isTerminal = liftIO $ (&&) <$> queryTerminal (Fd 0) <*> queryTerminal (Fd 1)
