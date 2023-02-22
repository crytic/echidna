{-# LANGUAGE CPP #-}

module Echidna.UI.Widgets where

#ifdef INTERACTIVE_UI

import Brick
import Brick.AttrMap qualified as A
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.Reader (MonadReader, asks)
import Data.List (nub, intersperse, sortBy)
import Data.Text qualified as T
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

data UIState = Uninitialized | Running | Timedout | Crashed String

attrs :: A.AttrMap
attrs = A.attrMap (V.white `on` V.black)
  [ (attrName "failure", fg V.brightRed)
  , (attrName "maximum", fg V.brightBlue)
  , (attrName "bold", fg V.white `V.withStyle` V.bold)
  , (attrName "tx", fg V.brightWhite)
  , (attrName "working", fg V.brightBlue)
  , (attrName "success", fg V.brightGreen)
  ]

data Name =
  TestsViewPort
  | SBClick ClickableScrollbarElement Name
  deriving (Ord, Show, Eq)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: MonadReader EConfig m
               => (Campaign, UIState) -> m (Widget Name)
campaignStatus (c@Campaign{_tests, _coverage, _ncallseqs}, uiState) = do
  done <- isDone c
  case (uiState, done) of
    (Uninitialized, _) ->
      pure $ mainbox (padLeft (Pad 1) $ str "Starting up, please wait...") emptyWidget
    (Crashed e, _) ->
      pure $ mainbox (padLeft (Pad 1) $
        withAttr (attrName "failure") $ strBreak $ formatCrashReport e) emptyWidget
    (Timedout, _) ->
      mainboxTests (str "Timed out, C-c or esc to exit")
    (_, True) ->
      mainboxTests (str "Campaign complete, C-c or esc to exit")
    _ ->
      mainboxTests emptyWidget
  where
    mainboxTests underneath = do
      t <- testsWidget _tests
      pure $ mainbox (summaryWidget c <=> hBorderWithLabel (str "Tests") <=> t) underneath

    mainbox :: Widget Name -> Widget Name -> Widget Name
    mainbox inner underneath =
      padTop (Pad 1) $ hCenter $ hLimit 120 $
      wrapInner inner
      <=>
      hCenter underneath
    wrapInner = borderWithLabel (withAttr (attrName "bold") $ str title)
    title = "Echidna " ++ showVersion Paths_echidna.version

formatCrashReport :: String -> String
formatCrashReport e =
  "Echidna crashed with an error:\n\n" <>
  e <>
  "\n\nPlease report it to https://github.com/crytic/echidna/issues"

summaryWidget :: Campaign -> Widget Name
summaryWidget c =
  padLeft (Pad 1) (
      str ("Tests found: " ++ show (length c._tests)) <=>
      str ("Seed: " ++ show c._genDict.defSeed)
    <=>
    maybe emptyWidget str (ppCoverage c._coverage)
    <=>
    maybe emptyWidget str (ppCorpus c._corpus)
  )

failedFirst :: EchidnaTest -> EchidnaTest -> Ordering
failedFirst t1 _ | didFailed t1 = LT
                 | otherwise   = GT

testsWidget :: MonadReader EConfig m => [EchidnaTest] -> m (Widget Name)
testsWidget tests' =
  withClickableVScrollBars SBClick .
  withVScrollBars OnRight .
  withVScrollBarHandles .
  viewport TestsViewPort Vertical .
  foldl (<=>) emptyWidget . intersperse hBorder <$>
    traverse testWidget (sortBy failedFirst tests')

testWidget :: MonadReader EConfig m => EchidnaTest -> m (Widget Name)
testWidget etest =
 case etest.testType of
      Exploration           -> widget tsWidget "exploration" ""
      PropertyTest n _      -> widget tsWidget n ""
      OptimizationTest n _  -> widget optWidget n "optimizing "
      AssertionTest _ s _   -> widget tsWidget (encodeSig s) "assertion in "
      CallTest n _          -> widget tsWidget n ""

  where
  widget f n infront = do
    (status, details) <- f (etest.testState) etest
    pure $ padLeft (Pad 1) $
      str infront <+> name n <+> str ": " <+> status
      <=> padTop (Pad 1) details
  name n = withAttr (attrName "bold") $ str (T.unpack n)

tsWidget :: MonadReader EConfig m
         => TestState -> EchidnaTest -> m (Widget Name, Widget Name)
tsWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
tsWidget Solved     t = failWidget Nothing t.testReproducer t.testEvents t.testValue t.testResult
tsWidget Passed     _ = pure (withAttr (attrName "success") $ str "PASSED!", emptyWidget)
tsWidget (Open i)   t = do
  n <- asks (.campaignConf.testLimit)
  if i >= n then
    tsWidget Passed t
  else
    pure (withAttr (attrName "working") $ str $ "fuzzing " ++ progress i n, emptyWidget)
tsWidget (Large n)  t = do
  m <- asks (.campaignConf.shrinkLimit)
  failWidget (if n < m then Just (n,m) else Nothing) t.testReproducer t.testEvents t.testValue t.testResult

titleWidget :: Widget n
titleWidget = str "Call sequence" <+> str ":"

eventWidget :: Events -> Widget n
eventWidget es =
  if null es then str ""
  else str "Event sequence" <+> str ":"
       <=> strBreak (T.unpack $ T.intercalate "\n" es)

failWidget :: MonadReader EConfig m
           => Maybe (Int, Int) -> [Tx] -> Events -> TestValue -> TxResult -> m (Widget Name, Widget Name)
failWidget _ [] _  _  _= pure (failureBadge, str "*no transactions made*")
failWidget b xs es _ r = do
  s <- seqWidget xs
  pure (failureBadge  <+> str (" with " ++ show r), status <=> titleWidget <=> s <=> eventWidget es)
  where
  status = case b of
    Nothing    -> emptyWidget
    Just (n,m) -> str "Current action: " <+> withAttr (attrName "working") (str ("shrinking " ++ progress n m))

optWidget :: MonadReader EConfig m
          => TestState -> EchidnaTest -> m (Widget Name, Widget Name)
optWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
optWidget Solved     _ = error "optimization tests cannot be solved"
optWidget Passed     t = pure (str $ "max value found: " ++ show t.testValue, emptyWidget)
optWidget (Open i)   t = do
  n <- asks (.campaignConf.testLimit)
  if i >= n then
    optWidget Passed t
  else
    pure (withAttr (attrName "working") $ str $ "optimizing " ++ progress i n ++ ", current max value: " ++ show t.testValue, emptyWidget)
optWidget (Large n)  t = do
  m <- asks (.campaignConf.shrinkLimit)
  maxWidget (if n < m then Just (n,m) else Nothing) t.testReproducer t.testEvents t.testValue

maxWidget :: MonadReader EConfig m
           => Maybe (Int, Int) -> [Tx] -> Events -> TestValue -> m (Widget Name, Widget Name)
maxWidget _ [] _  _ = pure (failureBadge, str "*no transactions made*")
maxWidget b xs es v = do
  s <- seqWidget xs
  pure (maximumBadge  <+> str (" max value: " ++ show v), status <=> titleWidget <=> s <=> eventWidget es)
  where
  status = case b of
    Nothing    -> emptyWidget
    Just (n,m) -> str "Current action: " <+> withAttr (attrName "working") (str ("shrinking " ++ progress n m))

seqWidget :: MonadReader EConfig m => [Tx] -> m (Widget Name)
seqWidget xs = do
    ppTxs <- mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs
    let ordinals = str . printf "%d." <$> [1 :: Int ..]
    pure $
      foldl (<=>) emptyWidget $
        zipWith (<+>) ordinals (withAttr (attrName "tx") . strBreak <$> ppTxs)

failureBadge :: Widget Name
failureBadge = withAttr (attrName "failure") $ str "FAILED!"

maximumBadge :: Widget Name
maximumBadge = withAttr (attrName "maximum") $ str "OPTIMIZED!"

strBreak :: String -> Widget n
strBreak = strWrapWith $ defaultWrapSettings { breakLongWords = True }

#endif
