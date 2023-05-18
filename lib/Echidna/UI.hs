{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Echidna.UI where

#ifdef INTERACTIVE_UI
import Brick
import Brick.BChan
import Brick.Widgets.Dialog qualified as B
import Data.Sequence ((|>))
import Graphics.Vty (Config, Event(..), Key(..), Modifier(..), defaultConfig, inputMap, mkVty)
import Graphics.Vty qualified as Vty
import System.Posix
import Echidna.UI.Widgets
#endif

import Control.Concurrent (killThread, threadDelay)
import Control.Exception (AsyncException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Random.Strict (MonadRandom)
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (state)
import Data.ByteString.Lazy qualified as BS
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Time
import UnliftIO
  ( MonadUnliftIO, newIORef, readIORef, atomicWriteIORef, hFlush, stdout
  , writeIORef, atomicModifyIORef', timeout
  )
import UnliftIO.Concurrent hiding (killThread, threadDelay)

import EVM (VM, Contract)
import EVM.Types (Addr, W256)

import Echidna.ABI
import Echidna.Async (addEventHandler, pushEventIO)
import Echidna.Campaign (runWorker)
import Echidna.Output.JSON qualified
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Corpus (corpusSize)
import Echidna.Types.Coverage (scoveragePoints)
import Echidna.Types.Test (EchidnaTest(..), didFail, isOptimizationTest, TestType, TestState(..))
import Echidna.Types.Tx (Tx)
import Echidna.Types.World (World)
import Echidna.UI.Report
import Echidna.Utility (timePrefix, getTimestamp)

data UIEvent =
  CampaignUpdated LocalTime [EchidnaTest] [WorkerState]
  | FetchCacheUpdated (Map Addr (Maybe Contract))
                      (Map Addr (Map W256 (Maybe W256)))
  | WorkerEvent (Int, LocalTime, CampaignEvent)

-- | Set up and run an Echidna 'Campaign' and display interactive UI or
-- print non-interactive output in desired format at the end
ui
  :: (MonadCatch m, MonadRandom m, MonadReader Env m, MonadUnliftIO m)
  => VM      -- ^ Initial VM state
  -> World   -- ^ Initial world state
  -> GenDict
  -> [[Tx]]
  -> m [WorkerState]
ui vm world dict initialCorpus = do
  env <- ask
  conf <- asks (.cfg)
  terminalPresent <- liftIO isTerminal

  let
    -- default to one worker if not configured
    nworkers = fromIntegral $ fromMaybe 1 conf.campaignConf.workers

    effectiveMode = case conf.uiConf.operationMode of
      Interactive | not terminalPresent -> NonInteractive Text
      other -> other

    -- Distribute over all workers, could be slightly bigger overall due to
    -- ceiling but this doesn't matter
    perWorkerTestLimit = ceiling
      (fromIntegral conf.campaignConf.testLimit / fromIntegral nworkers :: Double)

    chunkSize = ceiling
      (fromIntegral (length initialCorpus) / fromIntegral nworkers :: Double)
    corpusChunks = chunksOf chunkSize initialCorpus ++ repeat []

  workers <- forM (zip corpusChunks [0..(nworkers-1)]) $
    uncurry (spawnWorker env perWorkerTestLimit)

  case effectiveMode of
#ifdef INTERACTIVE_UI
    Interactive -> do
      -- Channel to push events to update UI
      uiChannel <- liftIO $ newBChan 1000
      let forwardEvent = writeBChan uiChannel . WorkerEvent
      addEventHandler forwardEvent

      ticker <- liftIO . forkIO . forever $ do
        threadDelay 200_000 -- 200 ms

        now <- getTimestamp
        tests <- readIORef env.testsRef
        states <- workerStates workers
        writeBChan uiChannel (CampaignUpdated now tests states)

        -- TODO: remove and use events for this
        c <- readIORef env.fetchContractCache
        s <- readIORef env.fetchSlotCache
        writeBChan uiChannel (FetchCacheUpdated c s)

      -- UI initialization
      let buildVty = do
            v <- mkVty =<< vtyConfig
            Vty.setMode (Vty.outputIface v) Vty.Mouse True
            pure v
      initialVty <- liftIO buildVty
      app <- customMain initialVty buildVty (Just uiChannel) <$> monitor

      liftIO $ do
        tests <- readIORef env.testsRef
        now <- getTimestamp
        void $ app UIState
          { campaigns = [initialWorkerState] -- ugly, fix me
          , workersAlive = nworkers
          , status = Uninitialized
          , timeStarted = now
          , timeStopped = Nothing
          , now = now
          , fetchedContracts = mempty
          , fetchedSlots = mempty
          , fetchedDialog = B.dialog (Just " Fetched contracts/slots ") Nothing 80
          , displayFetchedDialog = False
          , workerEvents = mempty
          , corpusSize = 0
          , coverage = 0
          , numCodehashes = 0
          , lastNewCov = now
          , tests
          }

      -- Exited from the UI, stop the workers, not needed anymore
      stopWorkers workers

      liftIO $ killThread ticker

      states <- workerStates workers
      liftIO . putStrLn =<< ppCampaign states

      pure states
#else
    Interactive -> error "Interactive UI is not available"
#endif

    NonInteractive outputFormat -> do
#ifdef INTERACTIVE_UI
      -- Handles ctrl-c, TODO: this doesn't work on Windows
      liftIO $ forM_ [sigINT, sigTERM] $ \sig ->
        installHandler sig (Catch $ stopWorkers workers) Nothing
#endif
      let forwardEvent = putStrLn . ppLogLine
      addEventHandler forwardEvent

      let printStatus = do
            states <- liftIO $ workerStates workers
            time <- timePrefix <$> getTimestamp
            line <- statusLine env states
            putStrLn $ time <> "[status] " <> line
            hFlush stdout

      ticker <- liftIO . forkIO . forever $ do
        threadDelay 3_000_000 -- 3 seconds
        printStatus

      liftIO $ killThread ticker

      -- print final status regardless the last scheduled update
      liftIO printStatus

      states <- liftIO $ workerStates workers

      case outputFormat of
        JSON ->
          liftIO $ BS.putStr =<< Echidna.Output.JSON.encodeCampaign env states
        Text -> do
          liftIO . putStrLn =<< ppCampaign states
        None ->
          pure ()
      pure states

  where

  spawnWorker env testLimit corpusChunk workerId = do
    stateRef <- newIORef initialWorkerState

    threadId <- forkIO $ do
      -- TODO: maybe figure this out with forkFinally?
      stopReason <- catches (do
          let timeoutUsecs = maybe (-1) (*1_000_000) env.cfg.uiConf.maxTime
          maybeResult <- timeout timeoutUsecs $
            runWorker (get >>= writeIORef stateRef)
                      vm world dict workerId corpusChunk testLimit
          pure $ case maybeResult of
            Just (stopReason, _finalState) -> stopReason
            Nothing -> TimeLimitReached
        )
        [ Handler $ \(e :: AsyncException) -> pure $ Killed (show e)
        , Handler $ \(e :: SomeException)  -> pure $ Crashed (show e)
        ]

      liftIO $ pushEventIO env workerId (WorkerStopped stopReason)

    pure (threadId, stateRef)

  -- | Get a snapshot of all worker states
  workerStates workers =
    forM workers $ \(_, stateRef) -> readIORef stateRef

#ifdef INTERACTIVE_UI
 -- | Order the workers to stop immediately
stopWorkers :: MonadIO m => [(ThreadId, a)] -> m ()
stopWorkers workers =
  forM_ workers $ \(threadId, _) -> liftIO $ killThread threadId

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

    onEvent = \case
      AppEvent (CampaignUpdated now tests c') ->
        modify' $ \state -> state { campaigns = c', status = Running, now, tests }
      AppEvent (FetchCacheUpdated contracts slots) ->
        modify' $ \state ->
          state { fetchedContracts = contracts
                , fetchedSlots = slots }
      AppEvent (WorkerEvent event@(_,time,campaignEvent)) -> do
        modify' $ \state -> state { workerEvents = state.workerEvents |> event }

        case campaignEvent of
          NewCoverage coverage numCodehashes size _ ->
            modify' $ \state ->
              state { coverage = max state.coverage coverage -- max not really needed
                    , corpusSize = size
                    , numCodehashes
                    , lastNewCov = time
                    }
          WorkerStopped _ ->
            modify' $ \state ->
              state { workersAlive = state.workersAlive - 1
                    , timeStopped = if state.workersAlive == 1
                                       then Just time else Nothing
                    }

          _ -> pure ()
      VtyEvent (EvKey (KChar 'f') _) ->
        modify' $ \state ->
          state { displayFetchedDialog = not state.displayFetchedDialog }
      VtyEvent (EvKey KEsc _)                         -> halt
      VtyEvent (EvKey (KChar 'c') l) | MCtrl `elem` l -> halt
      MouseDown (SBClick el n) _ _ _ ->
        case n of
          TestsViewPort -> do
            let vp = viewportScroll TestsViewPort
            case el of
              SBHandleBefore -> vScrollBy vp (-1)
              SBHandleAfter  -> vScrollBy vp 1
              SBTroughBefore -> vScrollBy vp (-10)
              SBTroughAfter  -> vScrollBy vp 10
              SBBar          -> pure ()
          LogViewPort -> do
            let vp = viewportScroll LogViewPort
            case el of
              SBHandleBefore -> vScrollBy vp (-1)
              SBHandleAfter  -> vScrollBy vp 1
              SBTroughBefore -> vScrollBy vp (-10)
              SBTroughAfter  -> vScrollBy vp 10
              SBBar          -> pure ()
          _ -> pure ()
      _ -> pure ()

  env <- ask
  pure $ App { appDraw = drawUI env
             , appStartEvent = pure ()
             , appHandleEvent = onEvent
             , appAttrMap = const attrs
             , appChooseCursor = neverShowCursor
             }
#endif

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: IO Bool
isTerminal =
#ifdef INTERACTIVE_UI
  (&&) <$> queryTerminal (Fd 0) <*> queryTerminal (Fd 1)
#else
  pure False
#endif

-- | Composes a compact text status line of the campaign
statusLine
  :: Env
  -> [WorkerState]
  -> IO String
statusLine env states = do
  tests <- readIORef env.testsRef
  points <- scoveragePoints =<< readIORef env.coverageRef
  corpus <- readIORef env.corpusRef
  let totalCalls = sum ((.ncalls) <$> states)
  pure $ "tests: " <> show (length $ filter didFail tests) <> "/" <> show (length tests)
    <> ", fuzzing: " <> show totalCalls <> "/" <> show env.cfg.campaignConf.testLimit
    <> ", values: " <> show ((.value) <$> filter isOptimizationTest tests)
    <> ", cov: " <> show points
    <> ", corpus: " <> show (corpusSize corpus)
