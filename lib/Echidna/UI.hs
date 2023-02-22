{-# LANGUAGE CPP #-}

module Echidna.UI where

#ifdef INTERACTIVE_UI
import Brick
import Brick.BChan
import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.Catch (MonadCatch(..), catchAll)
import Graphics.Vty qualified as V
import Graphics.Vty (Config, Event(..), Key(..), Modifier(..), defaultConfig, inputMap, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import UnliftIO.Concurrent (forkIO, forkFinally)

import Echidna.UI.Widgets
#else /* !INTERACTIVE_UI */
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.State.Strict (get)
#endif

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader, asks)
import Control.Monad.Random.Strict (MonadRandom)
import Data.ByteString.Lazy qualified as BS
import Data.IORef
import Data.Maybe (fromMaybe)
import UnliftIO (MonadUnliftIO)
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
import Echidna.Types.Config

data CampaignEvent =
  CampaignUpdated Campaign
  | CampaignTimedout Campaign
  | CampaignCrashed String

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
  let uiConf = conf.uiConf
  ref <- liftIO $ newIORef defaultCampaign
  let updateRef = get >>= liftIO . atomicWriteIORef ref
      secToUsec = (* 1000000)
      timeoutUsec = secToUsec $ fromMaybe (-1) uiConf.maxTime
      runCampaign = timeout timeoutUsec (campaign updateRef vm world ts d txs)
#ifdef INTERACTIVE_UI
  terminalPresent <- liftIO isTerminal
#endif
  let effectiveMode = case uiConf.operationMode of
#ifdef INTERACTIVE_UI
        Interactive | not terminalPresent -> NonInteractive Text
#else
        Interactive -> NonInteractive Text
#endif
        other -> other
  case effectiveMode of
#ifdef INTERACTIVE_UI
    Interactive -> do
      bc <- liftIO $ newBChan 100
      let updateUI e = readIORef ref >>= writeBChan bc . e
      ticker <- liftIO $ forkIO $ -- run UI update every 100ms
        forever $ threadDelay 100000 >> updateUI CampaignUpdated
      _ <- forkFinally -- run worker
        (void $ do
          catchAll
            (runCampaign >>= \case
              Nothing -> liftIO $ updateUI CampaignTimedout
              Just _ -> liftIO $ updateUI CampaignUpdated)
            (liftIO . writeBChan bc . CampaignCrashed . show)
        )
        (const $ liftIO $ killThread ticker)
      let buildVty = do
            v <- mkVty =<< vtyConfig
            V.setMode (V.outputIface v) V.Mouse True
            pure v
      initialVty <- liftIO buildVty
      app <- customMain initialVty buildVty (Just bc) <$> monitor
      liftIO $ void $ app (defaultCampaign, Uninitialized)
      final <- liftIO $ readIORef ref
      liftIO . putStrLn $ runReader (ppCampaign final) conf
      pure final
#else
    Interactive -> error "Interactive UI is not available"
#endif
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

#ifdef INTERACTIVE_UI

vtyConfig :: IO Config
vtyConfig = do
  config <- V.standardIOConfig
  pure config { inputMap = (Nothing, "\ESC[6;2~", EvKey KPageDown [MShift]) :
                           (Nothing, "\ESC[5;2~", EvKey KPageUp [MShift]) :
                           inputMap defaultConfig
              }

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: MonadReader Env m => m (App (Campaign, UIState) CampaignEvent Name)
monitor = do
  let drawUI :: EConfig -> (Campaign, UIState) -> [Widget Name]
      drawUI conf camp = [runReader (campaignStatus camp) conf]

      onEvent (AppEvent (CampaignUpdated c')) = put (c', Running)
      onEvent (AppEvent (CampaignTimedout c')) = put (c', Timedout)
      onEvent (AppEvent (CampaignCrashed e)) = do
        (c,_) <- get
        put (c, Crashed e)
      onEvent (VtyEvent (EvKey KEsc _))                         = halt
      onEvent (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = halt
      onEvent (MouseDown (SBClick el n) _ _ _) =
        case n of
          TestsViewPort -> do
            let vp = viewportScroll TestsViewPort
            case el of
              SBHandleBefore -> vScrollBy vp (-1)
              SBHandleAfter  -> vScrollBy vp 1
              SBTroughBefore -> vScrollBy vp (-10)
              SBTroughAfter  -> vScrollBy vp 10
              SBBar          -> pure ()
          _ -> pure ()
      onEvent _ = pure ()

  conf <- asks (.cfg)
  pure $ App { appDraw = drawUI conf
             , appStartEvent = pure ()
             , appHandleEvent = onEvent
             , appAttrMap = const attrs
             , appChooseCursor = neverShowCursor
             }

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: IO Bool
isTerminal = (&&) <$> queryTerminal (Fd 0) <*> queryTerminal (Fd 1)

#endif