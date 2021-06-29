{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.UI.Widgets where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import Control.Monad.Reader (MonadReader)
import Data.Has (Has(..))
import Data.List (nub, intersperse)
import Data.Version (showVersion)
import Text.Printf (printf)

import qualified Brick.AttrMap as A
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Paths_echidna (version)

import Echidna.ABI
import Echidna.Campaign (isDone)
import Echidna.Events (Events)
import Echidna.Types.Campaign
import Echidna.Types.Test
import Echidna.Types.Tx (Tx, TxResult(..), TxConf, src)
import Echidna.UI.Report

data UIState = Uninitialized | Running | Timedout

attrs :: A.AttrMap
attrs = A.attrMap (V.white `on` V.black)
  [ ("failure", fg V.brightRed)
  , ("maximum", fg V.brightBlue)
  , ("bold", fg V.white `V.withStyle` V.bold)
  , ("tx", fg V.brightWhite)
  , ("working", fg V.brightBlue)
  , ("success", fg V.brightGreen)
  ]

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
               => (Campaign, UIState) -> m (Widget ())
campaignStatus (c@Campaign{_tests, _coverage, _ncallseqs}, uiState) = do
  done <- isDone c
  case (uiState, done) of
    (Uninitialized, _) -> pure $ mainbox (padLeft (Pad 1) $ str "Starting up, please wait...") emptyWidget
    (Timedout, _)      -> mainbox <$> testsWidget _tests <*> pure (str "Timed out, C-c or esc to exit")
    (_, True)          -> mainbox <$> testsWidget _tests <*> pure (str "Campaign complete, C-c or esc to exit")
    _                  -> mainbox <$> testsWidget _tests <*> pure emptyWidget
  where
    mainbox :: Widget () -> Widget () -> Widget ()
    mainbox inner underneath =
      padTop (Pad 1) $ hCenter $ hLimit 120 $
      wrapInner inner
      <=>
      hCenter underneath
    wrapInner inner =
      borderWithLabel (withAttr "bold" $ str title) $
      summaryWidget c
      <=>
      hBorderWithLabel (str "Tests")
      <=>
      inner
    title = "Echidna " ++ showVersion Paths_echidna.version

summaryWidget :: Campaign -> Widget ()
summaryWidget c =
  padLeft (Pad 1) (
      str ("Tests found: " ++ show (length $ c ^. tests)) <=>
      str ("Seed: " ++ show (c ^. genDict . defSeed))
    <=>
    maybe emptyWidget str (ppCoverage $ c ^. coverage)
    <=>
    maybe emptyWidget str (ppCorpus $ c ^. corpus)
  )

testsWidget :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
            => [EchidnaTest] -> m (Widget())
testsWidget tests' = foldl (<=>) emptyWidget . intersperse hBorder <$> traverse testWidget tests'

testWidget :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
           => EchidnaTest -> m (Widget ())
testWidget etest =
 case (etest ^. testType) of
      Exploration           -> widget tsWidget "exploration" ""
      PropertyTest n _      -> widget tsWidget n ""
      OptimizationTest n _  -> widget optWidget n "optimizing " 
      AssertionTest s _     -> widget tsWidget (encodeSig s) "assertion in "
      CallTest n _          -> widget tsWidget n ""
 
  where
  widget f n infront = do
    (status, details) <- f (etest ^. testState) etest
    pure $ padLeft (Pad 1) $
      str infront <+> name n <+> str ": " <+> status
      <=> padTop (Pad 1) details
  name n = withAttr "bold" $ str (T.unpack n)

tsWidget :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
         => TestState -> EchidnaTest -> m (Widget (), Widget ())
tsWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
tsWidget (Solved)   t = failWidget Nothing (t ^. testReproducer) (t ^. testEvents) (t ^. testValue) (t  ^. testResult)
tsWidget Passed     t = pure (withAttr "success" $ str "PASSED!", emptyWidget)
tsWidget (Open i)   t = do
  n <- view (hasLens . testLimit)
  if i >= n then
    tsWidget Passed t
  else
    pure (withAttr "working" $ str $ "fuzzing " ++ progress i n, emptyWidget)
tsWidget (Large n)  t = do
  m <- view (hasLens . shrinkLimit)
  failWidget (if n < m then Just (n,m) else Nothing) (t ^. testReproducer) (t ^. testEvents) (t ^. testValue) (t  ^. testResult)

failWidget :: (MonadReader x m, Has Names x, Has TxConf x)
           => Maybe (Int, Int) -> [Tx] -> Events -> TestValue -> TxResult -> m (Widget (), Widget ())
failWidget _ [] _  _  _= pure (failureBadge, str "*no transactions made*")
failWidget b xs es _ r = do
  s <- seqWidget xs
  pure (failureBadge  <+> str (" with " ++ show r), status <=> titleWidget <=> s <=> eventWidget)
  where
  titleWidget  = str "Call sequence" <+> str ":"
  eventWidget = if null es then error "no events!" else (str "Event sequence" <+> str ":" <=> str (T.unpack $ T.intercalate ", " es))

  status = case b of
    Nothing    -> emptyWidget
    Just (n,m) -> str "Current action: " <+> withAttr "working" (str ("shrinking " ++ progress n m))


optWidget :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
         => TestState -> EchidnaTest -> m (Widget (), Widget ())
optWidget (Failed e) _ = pure (str "could not evaluate", str $ show e)
optWidget (Solved)   _ = error "optimization tests cannot be solved"
optWidget Passed     t = pure (str $ "max value found: " ++ show (t ^. testValue), emptyWidget)
optWidget (Open i)   t = do
  n <- view (hasLens . testLimit)
  if i >= n then
    optWidget Passed t
  else
    pure (withAttr "working" $ str $ "optimizing " ++ progress i n ++ ", current max value: " ++ show (t ^. testValue), emptyWidget)
optWidget (Large n)  t = do
  m <- view (hasLens . shrinkLimit)
  maxWidget (if n < m then Just (n,m) else Nothing) (t ^. testReproducer) (t ^. testEvents) (t ^. testValue) (t  ^. testResult)

maxWidget :: (MonadReader x m, Has Names x, Has TxConf x)
           => Maybe (Int, Int) -> [Tx] -> Events -> TestValue -> TxResult -> m (Widget (), Widget ())
maxWidget _ [] _  _  _= pure (failureBadge, str "*no transactions made*")
maxWidget b xs es v r = do
  s <- seqWidget xs
  pure (maximumBadge  <+> str (" max value: " ++ show v), status <=> titleWidget <=> s <=> eventWidget)
  where
  titleWidget  = str "Call sequence" <+> str ":"
  eventWidget = if null es then str "" else str "Event sequence" <+> str ":" <=> str (T.unpack $ T.intercalate ", " es)

  status = case b of
    Nothing    -> emptyWidget
    Just (n,m) -> str "Current action: " <+> withAttr "working" (str ("shrinking " ++ progress n m))


seqWidget :: (MonadReader x m, Has Names x, Has TxConf x) => [Tx] -> m (Widget ())
seqWidget xs = do
    ppTxs <- mapM (ppTx $ length (nub $ view src <$> xs) /= 1) xs
    let ordinals = str . printf "%d." <$> [1 :: Int ..]
    pure $
      foldl (<=>) emptyWidget $
        zipWith (<+>) ordinals (withAttr "tx" . strWrap <$> ppTxs)

failureBadge :: Widget ()
failureBadge = withAttr "failure" $ str "FAILED!"

maximumBadge :: Widget ()
maximumBadge = withAttr "maximum" $ str "OPTIMIZED!"
