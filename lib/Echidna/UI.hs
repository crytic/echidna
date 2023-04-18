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
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (state)
import Control.Monad.ST (RealWorld)
import Data.ByteString.Lazy qualified as BS
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time
import UnliftIO
  ( MonadUnliftIO, IORef, newIORef, readIORef, hFlush, stdout , writeIORef, timeout)
import UnliftIO.Concurrent hiding (killThread, threadDelay)

import EVM.Solidity (SolcContract)
import EVM.Types (Addr, Contract, VM, VMType(Concrete), W256)

import Echidna.ABI
import Echidna.Campaign (runWorker, spawnListener)
import Echidna.Output.Corpus (saveCorpusEvent)
import Echidna.Output.JSON qualified
import Echidna.Server (runSSEServer)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Corpus qualified as Corpus
import Echidna.Types.Coverage (scoveragePoints)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test (EchidnaTest(..), didFail, isOptimizationTest)
import Echidna.Types.Tx (Tx)
import Echidna.Types.World (World)
import Echidna.UI.Report
import Echidna.Utility (timePrefix, getTimestamp)

data UIEvent =
  CampaignUpdated LocalTime [EchidnaTest] [WorkerState]
  | FetchCacheUpdated (Map Addr (Maybe Contract))
                      (Map Addr (Map W256 (Maybe W256)))
  | EventReceived (LocalTime, CampaignEvent)

-- | Set up and run an Echidna 'Campaign' and display interactive UI or
-- print non-interactive output in desired format at the end
ui
  :: (MonadCatch m, MonadReader Env m, MonadUnliftIO m)
  => VM Concrete RealWorld -- ^ Initial VM state
  -> World   -- ^ Initial world state
  -> GenDict
  -> [(FilePath, [Tx])]
  -> Maybe Text
  -> [SolcContract]
  -> m [WorkerState]
ui vm world dict initialCorpus cliSelectedContract cs = do
  env <- ask
  conf <- asks (.cfg)
  terminalPresent <- liftIO isTerminal

  let
    nFuzzWorkers = getNFuzzWorkers conf.campaignConf
    nworkers = getNWorkers conf.campaignConf

    effectiveMode = case conf.uiConf.operationMode of
      Interactive | not terminalPresent -> NonInteractive Text
      other -> other

    -- Distribute over all workers, could be slightly bigger overall due to
    -- ceiling but this doesn't matter
    perWorkerTestLimit = ceiling
      (fromIntegral conf.campaignConf.testLimit / fromIntegral nFuzzWorkers :: Double)

    chunkSize = ceiling
      (fromIntegral (length initialCorpus) / fromIntegral nFuzzWorkers :: Double)
    corpusChunks = chunksOf chunkSize initialCorpus ++ repeat []

  corpusSaverStopVar <- spawnListener (saveCorpusEvent env)

  workers <- forM (zip corpusChunks [0..(nworkers-1)]) $
    uncurry (spawnWorker env perWorkerTestLimit)

  case effectiveMode of
#ifdef INTERACTIVE_UI
    Interactive -> do
      -- Channel to push events to update UI
      uiChannel <- liftIO $ newBChan 1000
      let forwardEvent = writeBChan uiChannel . EventReceived
      uiEventsForwarderStopVar <- spawnListener forwardEvent

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
          , fetchedDialog = B.dialog (Just $ str " Fetched contracts/slots ") Nothing 80
          , displayFetchedDialog = False
          , displayLogPane = True
          , displayTestsPane = True
          , events = mempty
          , corpusSize = 0
          , coverage = 0
          , numCodehashes = 0
          , lastNewCov = now
          , tests
          }

      -- Exited from the UI, stop the workers, not needed anymore
      stopWorkers workers

      -- wait for all events to be processed
      forM_ [uiEventsForwarderStopVar, corpusSaverStopVar] takeMVar

      liftIO $ killThread ticker

      states <- workerStates workers
      liftIO . putStrLn =<< ppCampaign vm states

      pure states
#else
    Interactive -> error "Interactive UI is not available"
#endif

    NonInteractive outputFormat -> do
      serverStopVar <- newEmptyMVar
#ifdef INTERACTIVE_UI
      -- Handles ctrl-c, TODO: this doesn't work on Windows
      liftIO $ forM_ [sigINT, sigTERM] $ \sig ->
        let handler = Catch $ do
              stopWorkers workers
              void $ tryPutMVar serverStopVar ()
        in installHandler sig handler Nothing
#endif
      let forwardEvent = putStrLn . ppLogLine
      uiEventsForwarderStopVar <- spawnListener forwardEvent

      let printStatus = do
            states <- liftIO $ workerStates workers
            time <- timePrefix <$> getTimestamp
            line <- statusLine env states
            putStrLn $ time <> "[status] " <> line
            hFlush stdout

      case conf.campaignConf.serverPort of
        Just port -> liftIO $ runSSEServer serverStopVar env port nworkers
        Nothing -> pure ()

      ticker <- liftIO . forkIO . forever $ do
        threadDelay 3_000_000 -- 3 seconds
        printStatus

      -- wait for all events to be processed
      forM_ [uiEventsForwarderStopVar, corpusSaverStopVar] takeMVar

      liftIO $ killThread ticker

      -- print final status regardless the last scheduled update
      liftIO printStatus

      when (isJust conf.campaignConf.serverPort) $ do
        -- wait until we send all SSE events
        liftIO $ putStrLn "Waiting until all SSE are received..."
        readMVar serverStopVar

      states <- liftIO $ workerStates workers

      case outputFormat of
        JSON ->
          liftIO $ BS.putStr =<< Echidna.Output.JSON.encodeCampaign env states
        Text -> do
          liftIO . putStrLn =<< ppCampaign vm states
        None ->
          pure ()
      pure states

  where

  spawnWorker env testLimit corpusChunk workerId = do
    stateRef <- newIORef initialWorkerState

    threadId <- forkIO $ do
      -- TODO: maybe figure this out with forkFinally?
      let workerType = workerIDToType env.cfg.campaignConf workerId
      stopReason <- catches (do
          let
            timeoutUsecs = maybe (-1) (*1_000_000) env.cfg.uiConf.maxTime
            corpus = if workerType == SymbolicWorker then initialCorpus else corpusChunk
          maybeResult <- timeout timeoutUsecs $
            runWorker workerType (get >>= writeIORef stateRef)
                      vm world dict workerId corpus testLimit cliSelectedContract cs
          pure $ case maybeResult of
            Just (stopReason, _finalState) -> stopReason
            Nothing -> TimeLimitReached
        )
        [ Handler $ \(e :: AsyncException) -> pure $ Killed (show e)
        , Handler $ \(e :: SomeException)  -> pure $ Crashed (show e)
        ]

      time <- liftIO getTimestamp
      writeChan env.eventQueue (time, WorkerEvent workerId workerType (WorkerStopped stopReason))

    pure (threadId, stateRef)

  -- | Get a snapshot of all worker states
  workerStates workers =
    forM workers $ \(_, stateRef) -> readIORef stateRef

#ifdef INTERACTIVE_UI
 -- | Order the workers to stop immediately
stopWorkers :: MonadIO m => [(ThreadId, IORef WorkerState)] -> m ()
stopWorkers workers =
  forM_ workers $ \(threadId, workerStateRef) -> do
    workerState <- readIORef workerStateRef
    liftIO $ mapM_ killThread (threadId : workerState.runningThreads)

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
      AppEvent (EventReceived event@(time,campaignEvent)) -> do
        modify' $ \state -> state { events = state.events |> event }

        case campaignEvent of
          WorkerEvent _ _ (NewCoverage { points, numCodehashes, corpusSize }) ->
            modify' $ \state ->
              state { coverage = max state.coverage points -- max not really needed
                    , corpusSize
                    , numCodehashes
                    , lastNewCov = time
                    }
          WorkerEvent _ _ (WorkerStopped _) ->
            modify' $ \state ->
              state { workersAlive = state.workersAlive - 1
                    , timeStopped = if state.workersAlive == 1
                                       then Just time else Nothing
                    }

          _ -> pure ()
      VtyEvent (EvKey (KChar 'f') _) ->
        modify' $ \state ->
          state { displayFetchedDialog = not state.displayFetchedDialog }
      VtyEvent (EvKey (KChar 'l') _) ->
        modify' $ \state ->
          state { displayLogPane = not state.displayLogPane }
      VtyEvent (EvKey (KChar 't') _) ->
        modify' $ \state ->
          state { displayTestsPane = not state.displayTestsPane }
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
    <> ", corpus: " <> show (Corpus.corpusSize corpus)

