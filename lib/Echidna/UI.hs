{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Echidna.UI where

#ifdef INTERACTIVE_UI
import Brick
import Brick.BChan
import Brick.Widgets.Dialog qualified as B
import Control.Monad.Catch (MonadCatch(..), catchAll)
import Control.Monad.Reader (MonadReader (ask), runReader, asks)
import Control.Monad.State (modify')
import Graphics.Vty (Config, Event(..), Key(..), Modifier(..), defaultConfig, inputMap, mkVty)
import Graphics.Vty qualified as Vty
import System.Posix

import Echidna.UI.Widgets
#else /* !INTERACTIVE_UI */
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State.Strict (get)
#endif

import Control.Monad
import Control.Concurrent (killThread, threadDelay)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Random.Strict (MonadRandom)
import Data.ByteString.Lazy qualified as BS
import Data.IORef
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Time (UTCTime, getCurrentTime)
import Data.Vector.Unboxed qualified as VU
import UnliftIO (MonadUnliftIO, hFlush, stdout)
import UnliftIO.Timeout (timeout)
import UnliftIO.Concurrent hiding (killThread, threadDelay)

import EVM (VM, Contract)
import EVM.Types (Addr, W256)

import Echidna.ABI
import Echidna.Campaign (runCampaign)
import Echidna.Output.JSON qualified
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Corpus (corpusSize)
import Echidna.Types.Coverage (scoveragePoints)
import Echidna.Types.Test (EchidnaTest(..), TestState(..), didFail, isOpen, isOptimizationTest)
import Echidna.Types.Tx (Tx)
import Echidna.Types.World (World)
import Echidna.UI.Report
import Echidna.Utility (timePrefix)

data UIEvent =
  CampaignUpdated UTCTime FrozenCampaign
  | CampaignTimedout FrozenCampaign
  | CampaignCrashed String
  | FetchCacheUpdated (Map Addr (Maybe Contract)) (Map Addr (Map W256 (Maybe W256)))

-- | Set up and run an Echidna 'Campaign' and display interactive UI or
-- print non-interactive output in desired format at the end
ui
  :: (MonadCatch m, MonadRandom m, MonadReader Env m, MonadUnliftIO m)
  => VM             -- ^ Initial VM state
  -> World          -- ^ Initial world state
  -> [EchidnaTest]  -- ^ Tests to evaluate
  -> GenDict
  -> [[Tx]]
  -> m Campaign
ui vm world ts dict initialCorpus = do
  conf <- asks (.cfg)
  ref <- liftIO $ newIORef defaultCampaign
  stop <- newEmptyMVar
  let
    updateRef = do
      shouldStop <- liftIO $ isJust <$> tryReadMVar stop
      get >>= liftIO . atomicWriteIORef ref
      pure shouldStop

    secToUsec = (* 1000000)
    timeoutUsec = secToUsec $ fromMaybe (-1) conf.uiConf.maxTime
    runCampaign' = timeout timeoutUsec (runCampaign updateRef vm world ts dict initialCorpus)
#ifdef INTERACTIVE_UI
  terminalPresent <- liftIO isTerminal
#else
  let terminalPresent = False
#endif
  let effectiveMode = case conf.uiConf.operationMode of
        Interactive | not terminalPresent -> NonInteractive Text
        other -> other
  case effectiveMode of
#ifdef INTERACTIVE_UI
    Interactive -> do
      bc <- liftIO $ newBChan 100
      let updateUI e = readIORef ref >>= freezeCampaign >>= writeBChan bc . e
      env <- ask
      ticker <- liftIO $ forkIO $
        -- run UI update every 100ms
        forever $ do
          threadDelay 100000
          now <- getCurrentTime
          updateUI (CampaignUpdated now)
          c <- readIORef env.fetchContractCache
          s <- readIORef env.fetchSlotCache
          writeBChan bc (FetchCacheUpdated c s)
      _ <- forkFinally -- run worker
        (void $ do
          catchAll
            (runCampaign' >>= \case
              Nothing -> liftIO $ updateUI CampaignTimedout
              Just _ -> liftIO $ do
                now <- getCurrentTime
                updateUI (CampaignUpdated now)
            )
            (liftIO . writeBChan bc . CampaignCrashed . show)
        )
        (const $ liftIO $ killThread ticker)
      let buildVty = do
            v <- mkVty =<< vtyConfig
            Vty.setMode (Vty.outputIface v) Vty.Mouse True
            pure v
      initialVty <- liftIO buildVty
      app <- customMain initialVty buildVty (Just bc) <$> monitor
      liftIO $ do
        now <- getCurrentTime
        void $ app UIState
          { campaign = defaultCampaign
          , status = Uninitialized
          , timeStarted = now
          , now = now
          , fetchedContracts = mempty
          , fetchedSlots = mempty
          , fetchedDialog = B.dialog (Just "Fetched contracts/slots") Nothing 80
          , displayFetchedDialog = False
          }
      final <- liftIO $ readIORef ref
      liftIO . putStrLn =<< ppCampaign final
      pure final
#else
    Interactive -> error "Interactive UI is not available"
#endif

    NonInteractive outputFormat -> do
#ifdef INTERACTIVE_UI
      liftIO $ forM_ [sigINT, sigTERM] (\sig -> installHandler sig (Catch $ putMVar stop ()) Nothing)
#endif

      ticker <- liftIO $ forkIO $
        -- print out status update every 3s
        forever $ do
          threadDelay $ 3*1000000
          camp <- readIORef ref
          time <- timePrefix
          line <- statusLine conf.campaignConf camp
          putStrLn $ time <> "[status] " <> line
          hFlush stdout
      result <- runCampaign'
      liftIO $ killThread ticker
      (final, timedout) <- case result of
        Nothing -> do
          final <- liftIO $ readIORef ref
          pure (final, True)
        Just final ->
          pure (final, False)
      case outputFormat of
        JSON ->
          liftIO $ BS.putStr =<< Echidna.Output.JSON.encodeCampaign final
        Text -> do
          liftIO . putStrLn =<< ppCampaign final
          when timedout $ liftIO $ putStrLn "TIMEOUT!"
        None ->
          pure ()
      pure final

freezeCampaign :: Campaign -> IO FrozenCampaign
freezeCampaign camp = do
  frozenCov <- mapM VU.freeze camp.coverage
  pure camp { coverage = frozenCov }

#ifdef INTERACTIVE_UI

vtyConfig :: IO Config
vtyConfig = do
  config <- Vty.standardIOConfig
  pure config { inputMap = (Nothing, "\ESC[6;2~", EvKey KPageDown [MShift]) :
                           (Nothing, "\ESC[5;2~", EvKey KPageUp [MShift]) :
                           inputMap defaultConfig }

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: MonadReader Env m => m (App UIState UIEvent Name)
monitor = do
  let
    drawUI :: Env -> UIState -> [Widget Name]
    drawUI conf uiState =
      [ if uiState.displayFetchedDialog
           then fetchedDialogWidget uiState
           else emptyWidget
      , runReader (campaignStatus uiState) conf ]

    onEvent (AppEvent (CampaignUpdated now c')) =
      modify' $ \state -> state { campaign = c', status = Running, now = now }
    onEvent (AppEvent (CampaignTimedout c')) =
      modify' $ \state -> state { campaign = c', status = Timedout }
    onEvent (AppEvent (CampaignCrashed e)) = do
      modify' $ \state -> state { status = Crashed e }
    onEvent (AppEvent (FetchCacheUpdated contracts slots)) =
      modify' $ \state -> state { fetchedContracts = contracts
                                , fetchedSlots = slots }
    onEvent (VtyEvent (EvKey (KChar 'f') _)) =
      modify' $ \state -> state { displayFetchedDialog = not state.displayFetchedDialog }
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

  env <- ask
  pure $ App { appDraw = drawUI env
             , appStartEvent = pure ()
             , appHandleEvent = onEvent
             , appAttrMap = const attrs
             , appChooseCursor = neverShowCursor
             }

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: IO Bool
isTerminal = (&&) <$> queryTerminal (Fd 0) <*> queryTerminal (Fd 1)

#endif

-- | Composes a compact text status line of the campaign
statusLine :: CampaignConf -> Campaign -> IO String
statusLine campaignConf camp = do
  points <- scoveragePoints camp.coverage
  pure $ "tests: " <> show (length $ filter didFail camp.tests) <> "/" <> show (length camp.tests)
    <> ", fuzzing: " <> show fuzzRuns <> "/" <> show campaignConf.testLimit
    <> ", values: " <> show (map (.value) $ filter (\t -> isOptimizationTest t.testType) camp.tests)
    <> ", cov: " <> show points
    <> ", corpus: " <> show (corpusSize camp.corpus)
  where
  fuzzRuns = case filter isOpen camp.tests of
    -- fuzzing progress is the same for all Open tests, grab the first one
    EchidnaTest { state = Open t }:_ -> t
    _ -> campaignConf.testLimit
