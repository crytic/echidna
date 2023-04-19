{-# LANGUAGE CPP #-}

module Echidna.UI.Widgets where

#ifdef INTERACTIVE_UI

import Brick
import Brick.AttrMap qualified as A
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.Reader (MonadReader, asks)
import Data.List (nub, intersperse, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Time (UTCTime, NominalDiffTime, formatTime, defaultTimeLocale, diffUTCTime)
import Data.Version (showVersion)
import Graphics.Vty qualified as V
import Paths_echidna qualified (version)
import Text.Printf (printf)
import Text.Wrap

import Echidna.ABI
import Echidna.Campaign (isDone)
import Echidna.Events (Events)
import Echidna.Types.Campaign
import Echidna.Types.Test
import Echidna.Types.Tx (Tx(..), TxResult(..))
import Echidna.UI.Report
import Echidna.Types.Config
import Data.Map (Map)
import EVM.Types (Addr, W256)
import EVM (Contract)
import Brick.Widgets.Dialog qualified as B

data UIState = UIState
  { status :: UIStateStatus
  , campaign :: FrozenCampaign
  , timeStarted :: UTCTime
  , now :: UTCTime

  , fetchedContracts :: Map Addr (Maybe Contract)
  , fetchedSlots :: Map Addr (Map W256 (Maybe W256))
  , fetchedDialog :: B.Dialog ()
  , displayFetchedDialog :: Bool
  }

data UIStateStatus = Uninitialized | Running | Timedout | Crashed String

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
  ]

bold :: Widget n -> Widget n
bold = withAttr (attrName "bold")

failure :: Widget n -> Widget n
failure = withAttr (attrName "failure")

success :: Widget n -> Widget n
success = withAttr (attrName "success")

data Name =
  TestsViewPort
  | SBClick ClickableScrollbarElement Name
  deriving (Ord, Show, Eq)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: MonadReader Env m => UIState -> m (Widget Name)
campaignStatus uiState = do
  done <- isDone uiState.campaign
  case (uiState.status, done) of
    (Uninitialized, _) ->
      mainbox (padLeft (Pad 1) $ str "Starting up, please wait...") emptyWidget
    (Crashed e, _) ->
      mainbox (padLeft (Pad 1) $ failure $ strBreak $ formatCrashReport e) emptyWidget
    (Timedout, _) -> do
      tests <- testsWidget uiState.campaign.tests
      mainbox tests (finalStatus "Timed out, C-c or esc to exit")
    (_, True) -> do
      tests <- testsWidget uiState.campaign.tests
      mainbox tests (finalStatus "Campaign complete, C-c or esc to exit")
    _ -> do
      tests <- testsWidget uiState.campaign.tests
      mainbox tests emptyWidget
  where
  mainbox inner underneath =
    hCenter . hLimit 120 <$> wrapInner inner underneath
  wrapInner inner underneath = do
    chainId <- asks (.chainId)
    pure $ joinBorders $ borderWithLabel echidnaTitle $
      summaryWidget uiState chainId
      <=>
      hBorderWithLabel (withAttr (attrName "subtitle") $ str $
        (" Tests (" <> show (length uiState.campaign.tests)) <> ") ")
      <=>
      inner
      <=>
      underneath
  echidnaTitle =
    str "[ " <+>
    withAttr (attrName "title")
      (str $ "Echidna " <> showVersion Paths_echidna.version) <+>
    str " ]"
  finalStatus s = hBorder <=> hCenter (bold $ str s)

formatCrashReport :: String -> String
formatCrashReport e =
  "Echidna crashed with an error:\n\n" <>
  e <>
  "\n\nPlease report it to https://github.com/crytic/echidna/issues"

summaryWidget :: UIState -> Maybe W256 -> Widget Name
summaryWidget uiState chainId =
  vLimit 3 $ -- limit to 3 rows
    hLimitPercent 33 leftSide <+> vBorder <+>
    hLimitPercent 50 middle <+> vBorder <+>
    rightSide
  where
  leftSide =
    let c = uiState.campaign in
    padLeft (Pad 1) $
      timeElapsedWidget uiState
      <=>
      str ("Seed: " <> show c.genDict.defSeed) <+> fill ' '
  middle =
    let c = uiState.campaign in
    padLeft (Pad 1) $
      str (ppFrozenCoverage c.coverage) <+> fill ' '
      <=>
      str (ppCorpus c.corpus)
  rightSide =
    padLeft (Pad 1) $
      (rpcInfoWidget uiState.fetchedContracts uiState.fetchedSlots chainId)

timeElapsedWidget :: UIState -> Widget n
timeElapsedWidget uiState =
  str "Time elapsed: " <+>
  str ((formatNominalDiffTime . diffUTCTime uiState.now) uiState.timeStarted)

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
        style = if length successful == length fetches then success else failure
    in style . str $ show (length successful) <> "/" <> show (length fetches)

fetchedDialogWidget :: UIState -> Widget n
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
tsWidget Solved     t = failWidget Nothing t.reproducer t.events t.value t.result
tsWidget Passed     _ = pure (success $ str "PASSED!", emptyWidget)
tsWidget (Open i)   t = do
  n <- asks (.cfg.campaignConf.testLimit)
  if i >= n then
    tsWidget Passed t
  else
    pure (withAttr (attrName "working") $ str $ "fuzzing " ++ progress i n, emptyWidget)
tsWidget (Large n)  t = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  failWidget (if n < m then Just (n,m) else Nothing) t.reproducer t.events t.value t.result

titleWidget :: Widget n
titleWidget = str "Call sequence" <+> str ":"

eventWidget :: Events -> Widget n
eventWidget es =
  if null es then str ""
  else str "Event sequence" <+> str ":"
       <=> strBreak (T.unpack $ T.intercalate "\n" es)

failWidget
  :: MonadReader Env m
  => Maybe (Int, Int)
  -> [Tx]
  -> Events
  -> TestValue
  -> TxResult
  -> m (Widget Name, Widget Name)
failWidget _ [] _  _  _= pure (failureBadge, str "*no transactions made*")
failWidget b xs es _ r = do
  s <- seqWidget xs
  pure
    ( failureBadge <+> str (" with " ++ show r)
    , status <=> titleWidget <=> s <=> eventWidget es
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
optWidget (Open i)   t = do
  n <- asks (.cfg.campaignConf.testLimit)
  if i >= n then
    optWidget Passed t
  else
    pure (withAttr (attrName "working") $ str $ "optimizing " ++ progress i n
      ++ ", current max value: " ++ show t.value, emptyWidget)
optWidget (Large n)  t = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  maxWidget (if n < m then Just (n,m) else Nothing) t.reproducer t.events t.value

maxWidget
  :: MonadReader Env m
  => Maybe (Int, Int)
  -> [Tx]
  -> Events
  -> TestValue
  -> m (Widget Name, Widget Name)
maxWidget _ [] _  _ = pure (failureBadge, str "*no transactions made*")
maxWidget b xs es v = do
  s <- seqWidget xs
  pure
    ( maximumBadge <+> str (" max value: " ++ show v)
    , status <=> titleWidget <=> s <=> eventWidget es
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

#endif
