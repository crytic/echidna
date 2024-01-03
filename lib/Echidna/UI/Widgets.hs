{-# LANGUAGE CPP #-}

module Echidna.UI.Widgets where

#ifdef INTERACTIVE_UI

import Brick hiding (style)
import Brick.AttrMap qualified as A
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog qualified as B
import Control.Monad.Reader (MonadReader, asks, ask)
import Control.Monad.ST (RealWorld)
import Data.List (nub, intersperse, sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime, NominalDiffTime, formatTime, defaultTimeLocale, diffLocalTime)
import Data.Version (showVersion)
import Graphics.Vty qualified as V
import Paths_echidna qualified (version)
import Text.Printf (printf)
import Text.Wrap

import Echidna.ABI
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Test
import Echidna.Types.Tx (Tx(..), TxResult(..))
import Echidna.UI.Report
import Echidna.Utility (timePrefix)

import EVM.Format (showTraceTree)
import EVM.Types (Addr, Contract, W256, VM(..))

data UIState = UIState
  { status :: UIStateStatus
  , campaigns :: [WorkerState]
  , timeStarted :: LocalTime
  , timeStopped :: Maybe LocalTime
  , now :: LocalTime
  , fetchedContracts :: Map Addr (Maybe Contract)
  , fetchedSlots :: Map Addr (Map W256 (Maybe W256))
  , fetchedDialog :: B.Dialog () Name
  , displayFetchedDialog :: Bool

  , workerEvents :: Seq (Int, LocalTime, CampaignEvent)
  , workersAlive :: Int

  , corpusSize :: Int
  , coverage :: Int
  , numCodehashes :: Int
  , lastNewCov :: LocalTime
  -- ^ last timestamp of 'NewCoverage' event

  , tests :: [EchidnaTest]
  }

data UIStateStatus = Uninitialized | Running

attrs :: A.AttrMap
attrs = A.attrMap (V.white `on` V.black)
  [ (attrName "failure", fg V.brightRed)
  , (attrName "maximum", fg V.brightBlue)
  , (attrName "bold", fg V.white `V.withStyle` V.bold)
  , (attrName "tx", fg V.brightWhite)
  , (attrName "working", fg V.brightBlue)
  , (attrName "success", fg V.brightGreen)
  , (attrName "title", fg V.brightYellow `V.withStyle` V.bold)
  , (attrName "subtitle", fg V.brightCyan `V.withStyle` V.bold)
  , (attrName "time", fg (V.rgbColor (0x70 :: Int) 0x70 0x70))
  ]

bold :: Widget n -> Widget n
bold = withAttr (attrName "bold")

failure :: Widget n -> Widget n
failure = withAttr (attrName "failure")

success :: Widget n -> Widget n
success = withAttr (attrName "success")

data Name
  = LogViewPort
  | TestsViewPort
  | SBClick ClickableScrollbarElement Name
  deriving (Ord, Show, Eq)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: MonadReader Env m => UIState -> m (Widget Name)
campaignStatus uiState = do
  tests <- testsWidget uiState.tests

  if uiState.workersAlive == 0 then
    mainbox tests (finalStatus "Campaign complete, C-c or esc to exit")
  else
    case uiState.status of
      Uninitialized ->
        mainbox (padLeft (Pad 1) $ str "Starting up, please wait...") emptyWidget
      Running ->
        mainbox tests emptyWidget
  where
  mainbox inner underneath = do
    env <- ask
    pure $ hCenter . hLimit 120 $
      joinBorders $ borderWithLabel echidnaTitle $
      summaryWidget env uiState
      <=>
      hBorderWithLabel (withAttr (attrName "subtitle") $ str $
        (" Tests (" <> show (length uiState.tests)) <> ") ")
      <=>
      inner
      <=>
      hBorderWithLabel (withAttr (attrName "subtitle") $ str $
        " Log (" <> show (length uiState.workerEvents) <> ") ")
      <=>
      logPane uiState
      <=>
      underneath
  echidnaTitle =
    str "[ " <+>
    withAttr (attrName "title")
      (str $ "Echidna " <> showVersion Paths_echidna.version) <+>
    str " ]"
  finalStatus s = hBorder <=> hCenter (bold $ str s)

logPane :: UIState -> Widget Name
logPane uiState =
  vLimitPercent 33 .
  padLeft (Pad 1) $
  withClickableVScrollBars SBClick .
  withVScrollBars OnRight .
  withVScrollBarHandles .
  viewport LogViewPort Vertical $
  foldl (<=>) emptyWidget (showLogLine <$> Seq.reverse uiState.workerEvents)

showLogLine :: (Int, LocalTime, CampaignEvent) -> Widget Name
showLogLine (workerId, time, event) =
  (withAttr (attrName "time") $ str $ (timePrefix time) <> "[Worker " <> show workerId <> "] ")
    <+> strBreak (ppCampaignEvent event)

summaryWidget :: Env -> UIState -> Widget Name
summaryWidget env uiState =
  vLimit 5 $ -- limit to 5 rows
    hLimitPercent 33 leftSide <+> vBorder <+>
    hLimitPercent 50 middle <+> vBorder <+>
    rightSide
  where
  leftSide =
    padLeft (Pad 1) $
      (str ("Time elapsed: " <> timeElapsed uiState uiState.timeStarted) <+> fill ' ')
      <=>
      (str "Workers: " <+> outOf uiState.workersAlive (length uiState.campaigns))
      <=>
      str ("Seed: " ++ ppSeed uiState.campaigns)
      <=>
      perfWidget uiState
      <=>
      str ("Total calls: " <> progress (sum $ (.ncalls) <$> uiState.campaigns)
                                     env.cfg.campaignConf.testLimit)
  middle =
    padLeft (Pad 1) $
      str ("Unique instructions: " <> show uiState.coverage)
      <=>
      str ("Unique codehashes: " <> show uiState.numCodehashes)
      <=>
      str ("Corpus size: " <> show uiState.corpusSize <> " seqs")
      <=>
      str ("New coverage: " <> timeElapsed uiState uiState.lastNewCov <> " ago") <+> fill ' '
  rightSide =
    padLeft (Pad 1) $
      (rpcInfoWidget uiState.fetchedContracts uiState.fetchedSlots env.chainId)

timeElapsed :: UIState -> LocalTime -> String
timeElapsed uiState since =
  formatNominalDiffTime $
    diffLocalTime (fromMaybe uiState.now uiState.timeStopped) since

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime diff =
  let fmt = if | diff < 60       -> "%Ss"
               | diff < 60*60    -> "%Mm %Ss"
               | diff < 24*60*60 -> "%Hh %Mm %Ss"
               | otherwise       -> "%dd %Hh %Mm %Ss"
  in formatTime defaultTimeLocale fmt diff

rpcInfoWidget
  :: Map Addr (Maybe Contract)
  -> Map Addr (Map W256 (Maybe W256))
  -> Maybe W256
  -> Widget Name
rpcInfoWidget contracts slots chainId =
  (str "Chain ID: " <+> str (maybe "-" show chainId))
  <=>
  (str "Fetched contracts: " <+> countWidget (Map.elems contracts))
  <=>
  (str "Fetched slots: " <+> countWidget (concat $ Map.elems (Map.elems <$> slots)))
  where
  countWidget fetches =
    let successful = filter isJust fetches
    in outOf (length successful) (length fetches)

outOf :: Int -> Int -> Widget n
outOf n m =
  let style = if n == m then success else failure
  in style . str $ progress n m

perfWidget :: UIState -> Widget n
perfWidget uiState =
  str $ "Calls/s: " <>
    if totalTime > 0
       then show $ totalCalls `div` totalTime
       else "-"
  where
  totalCalls = sum $ (.ncalls) <$> uiState.campaigns
  totalTime = round $
    diffLocalTime (fromMaybe uiState.now uiState.timeStopped)
                  uiState.timeStarted

ppSeed :: [WorkerState] -> String
ppSeed campaigns = show (head campaigns).genDict.defSeed

fetchedDialogWidget :: UIState -> Widget Name
fetchedDialogWidget uiState =
  B.renderDialog uiState.fetchedDialog $ padLeftRight 1 $
    foldl (<=>) emptyWidget (Map.mapWithKey renderContract uiState.fetchedContracts)
  where
  renderContract addr (Just _code) =
    bold (str (show addr))
    <=>
    renderSlots addr
  renderContract addr Nothing =
    bold $ failure (str (show addr))
  renderSlots addr =
    foldl (<=>) emptyWidget $
      Map.mapWithKey renderSlot (fromMaybe mempty $ Map.lookup addr uiState.fetchedSlots)
  renderSlot slot (Just value) =
    padLeft (Pad 1) $ strBreak (show slot <> " => " <> show value)
  renderSlot slot Nothing =
    padLeft (Pad 1) $ failure $ str (show slot)

failedFirst :: EchidnaTest -> EchidnaTest -> Ordering
failedFirst t1 _ | didFail t1 = LT
                 | otherwise  = GT

testsWidget :: MonadReader Env m => [EchidnaTest] -> m (Widget Name)
testsWidget tests' =
  withClickableVScrollBars SBClick .
  withVScrollBars OnRight .
  withVScrollBarHandles .
  viewport TestsViewPort Vertical .
  foldl (<=>) emptyWidget . intersperse hBorder <$>
    traverse testWidget (sortBy failedFirst tests')

testWidget :: MonadReader Env m => EchidnaTest -> m (Widget Name)
testWidget test =
  case test.testType of
    Exploration          -> widget tsWidget "exploration" ""
    PropertyTest n _     -> widget tsWidget n ""
    OptimizationTest n _ -> widget optWidget n "optimizing "
    AssertionTest _ s _  -> widget tsWidget (encodeSig s) "assertion in "
    CallTest n _         -> widget tsWidget n ""
  where
  widget f n infront = do
    (status, details) <- f test.state test
    pure $ padLeft (Pad 1) $
      str infront <+> name n <+> str ": " <+> status
      <=> padTop (Pad 1) details
  name n = bold $ str (T.unpack n)

tsWidget
  :: MonadReader Env m
  => TestState
  -> EchidnaTest
  -> m (Widget Name, Widget Name)
tsWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
tsWidget Solved     t = failWidget Nothing t.reproducer (fromJust t.vm) t.value t.result
tsWidget Passed     _ = pure (success $ str "PASSED!", emptyWidget)
tsWidget Open       _ = pure (success $ str "passing", emptyWidget)
tsWidget (Large n)  t = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  failWidget (if n < m then Just (n,m) else Nothing) t.reproducer (fromJust t.vm) t.value t.result

titleWidget :: Widget n
titleWidget = str "Call sequence" <+> str ":"

tracesWidget :: MonadReader Env m => VM RealWorld -> m (Widget n)
tracesWidget vm = do
  dappInfo <- asks (.dapp)
  -- TODO: showTraceTree does coloring with ANSI escape codes, we need to strip
  -- those because they break the Brick TUI. Fix in hevm so we can display
  -- colors here as well.
  let traces = stripAnsiEscapeCodes $ showTraceTree dappInfo vm
  pure $
    if T.null traces then str ""
    else str "Traces" <+> str ":" <=> (txtBreak traces)

failWidget
  :: MonadReader Env m
  => Maybe (Int, Int)
  -> [Tx]
  -> VM RealWorld
  -> TestValue
  -> TxResult
  -> m (Widget Name, Widget Name)
failWidget _ [] _  _  _= pure (failureBadge, str "*no transactions made*")
failWidget b xs vm _ r = do
  s <- seqWidget xs
  traces <- tracesWidget vm
  pure
    ( failureBadge <+> str (" with " ++ show r)
    , status <=> titleWidget <=> s <=> str " " <=> traces
    )
  where
  status = case b of
    Nothing -> emptyWidget
    Just (n,m) ->
      str "Current action: " <+>
        withAttr (attrName "working") (str ("shrinking " ++ progress n m))

optWidget
  :: MonadReader Env m
  => TestState
  -> EchidnaTest
  -> m (Widget Name, Widget Name)
optWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
optWidget Solved     _ = error "optimization tests cannot be solved"
optWidget Passed     t = pure (str $ "max value found: " ++ show t.value, emptyWidget)
optWidget Open       t =
  pure (withAttr (attrName "working") $ str $
    "optimizing, max value: " ++ show t.value, emptyWidget)
optWidget (Large n)  t = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  maxWidget (if n < m then Just (n,m) else Nothing) t.reproducer (fromJust t.vm) t.value

maxWidget
  :: MonadReader Env m
  => Maybe (Int, Int)
  -> [Tx]
  -> VM RealWorld
  -> TestValue
  -> m (Widget Name, Widget Name)
maxWidget _ [] _  _ = pure (failureBadge, str "*no transactions made*")
maxWidget b xs vm v = do
  s <- seqWidget xs
  traces <- tracesWidget vm
  pure
    ( maximumBadge <+> str (" max value: " ++ show v)
    , status <=> titleWidget <=> s <=> str " " <=> traces
    )
  where
  status = case b of
    Nothing -> emptyWidget
    Just (n,m) ->
      str "Current action: " <+>
        withAttr (attrName "working") (str ("shrinking " ++ progress n m))

seqWidget :: MonadReader Env m => [Tx] -> m (Widget Name)
seqWidget xs = do
  ppTxs <- mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs
  let ordinals = str . printf "%d." <$> [1 :: Int ..]
  pure $
    foldl (<=>) emptyWidget $
      zipWith (<+>) ordinals (withAttr (attrName "tx") . strBreak <$> ppTxs)

failureBadge :: Widget Name
failureBadge = failure $ str "FAILED!"

maximumBadge :: Widget Name
maximumBadge = withAttr (attrName "maximum") $ str "OPTIMIZED!"

strBreak :: String -> Widget n
strBreak = strWrapWith $ defaultWrapSettings { breakLongWords = True }

txtBreak :: Text -> Widget n
txtBreak = txtWrapWith $ defaultWrapSettings { breakLongWords = True }

#endif
