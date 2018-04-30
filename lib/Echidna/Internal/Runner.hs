{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Echidna.Internal.Runner (
    checkParallel
  ) where

import           Control.Concurrent.STM (TVar, atomically)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad (zipWithM, mzero)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))

import           Data.Bifunctor (first, second)
import qualified Data.Char as Char
import           Data.Either (partitionEithers, isRight)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe, catMaybes, isJust)
import           Data.Semigroup (Semigroup(..))

import           Hedgehog.Internal.Config
import           Hedgehog.Internal.Discovery (Pos(..), Position(..))
import qualified Hedgehog.Internal.Discovery as Discovery
import           Hedgehog.Internal.Gen (runDiscardEffect, runGenT)
import           Hedgehog.Internal.Property
  (Failure(..), Group(..), GroupName(..), Property(..), PropertyT(..),
    PropertyConfig(..), PropertyName(..), ShrinkLimit(..), ShrinkRetries(..),
    Log(..), Diff(..), runTestT)
import           Hedgehog.Internal.Queue
import           Hedgehog.Internal.Region
import           Hedgehog.Internal.Report
  (DiscardCount(..), FailedAnnotation(..), FailureReport(..), PropertyCount(..),
    Progress(..), Report(..), Result(..), ShrinkCount(..), TestCount(..))
import           Hedgehog.Internal.Runner (RunnerConfig(..))
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import           Hedgehog.Internal.Show
import           Hedgehog.Internal.Source
import           Hedgehog.Internal.Tree (Node(..), Tree(..), runTree)
import           Hedgehog.Range (Size)

import           System.Console.ANSI (ColorIntensity(..), Color(..))
import           System.Console.ANSI (ConsoleLayer(..), ConsoleIntensity(..))
import           System.Console.ANSI (SGR(..), setSGRCode)
import           System.Directory (makeRelativeToCurrentDirectory)

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as WL

checkParallel :: MonadIO m => Group -> m Bool
checkParallel =
  checkGroup
    RunnerConfig {
        runnerWorkers =
          Nothing
      , runnerColor =
          Nothing
      , runnerVerbosity =
          Nothing
      }

checkGroup :: MonadIO m => RunnerConfig -> Group -> m Bool
checkGroup config (Group group props) =
  liftIO $ do
    n <- resolveWorkers (runnerWorkers config)

    -- ensure few spare capabilities for concurrent-output, it's likely that
    -- our tests will saturate all the capabilities they're given.
    updateNumCapabilities (n + 2)

    putStrLn $ "━━━ " ++ unGroupName group ++ " ━━━"

    verbosity <- resolveVerbosity (runnerVerbosity config)
    summary <- checkGroupWith n verbosity (runnerColor config) props

    pure $
      summaryFailed summary == 0 &&
      summaryGaveUp summary == 0

checkGroupWith ::
     WorkerCount
  -> Verbosity
  -> Maybe UseColor
  -> [(PropertyName, Property)]
  -> IO Summary
checkGroupWith n verbosity mcolor props =
  displayRegion $ \sregion -> do
    svar <- atomically . TVar.newTVar $ mempty { summaryWaiting = PropertyCount (length props) }

    let
      start (TasksRemaining tasks) _ix (name, prop) =
        liftIO $ do
          updateSummary sregion svar mcolor $ \x -> x {
              summaryWaiting =
                PropertyCount tasks
            , summaryRunning =
                summaryRunning x + 1
            }

          atomically $ do
            region <-
              case verbosity of
                Quiet ->
                  newEmptyRegion
                Normal ->
                  newOpenRegion

            moveToBottom sregion

            pure (name, prop, region)

      finish (_name, _prop, _region) =
        updateSummary sregion svar mcolor $ \x -> x {
            summaryRunning =
              summaryRunning x - 1
          }

      finalize (_name, _prop, region) =
        finishRegion region

    summary <-
      fmap (mconcat . fmap (fromResult . reportStatus)) $
        runTasks n props start finish finalize $ \(name, prop, region) -> do
          result <- checkNamed region mcolor (Just name) prop
          updateSummary sregion svar mcolor
            (<> fromResult (reportStatus result))
          pure result

    updateSummary sregion svar mcolor (const summary)
    pure summary

checkNamed ::
     MonadIO m
  => Region
  -> Maybe UseColor
  -> Maybe PropertyName
  -> Property
  -> m (Report Result)
checkNamed region mcolor name prop = do
  seed <- liftIO Seed.random
  checkRegion region mcolor name 0 seed prop

checkRegion ::
     MonadIO m
  => Region
  -> Maybe UseColor
  -> Maybe PropertyName
  -> Size
  -> Seed
  -> Property
  -> m (Report Result)
checkRegion region mcolor name size seed prop =
  liftIO $ do
    result <-
      checkReport (propertyConfig prop) size seed (propertyTest prop) $ \progress -> do
        ppprogress <- renderProgress mcolor name progress
        case reportStatus progress of
          Running ->
            setRegion region ppprogress
          Shrinking _ ->
            openRegion region ppprogress

    ppresult <- renderResult mcolor name result
    case reportStatus result of
      Failed _ ->
        openRegion region ppresult
      GaveUp ->
        openRegion region ppresult
      OK ->
        setRegion region ppresult

    pure result

checkReport ::
     forall m.
     MonadIO m
  => MonadCatch m
  => PropertyConfig
  -> Size
  -> Seed
  -> PropertyT m ()
  -> (Report Progress -> m ())
  -> m (Report Result)
checkReport cfg size0 seed0 test0 updateUI =
  let
    test =
      catchAll test0 (fail . show)

    loop !tests !discards !size !seed = do
      updateUI $ Report tests discards Running

      if size > 99 then
        -- size has reached limit, reset to 0
        loop tests discards 0 seed

      else if tests >= fromIntegral (propertyTestLimit cfg) then
        -- we've hit the test limit, test was successful
        pure $ Report tests discards OK

      else if discards >= fromIntegral (propertyDiscardLimit cfg) then
        -- we've hit the discard limit, give up
        pure $ Report tests discards GaveUp

      else
        case Seed.split seed of
          (s0, s1) -> do
            node@(Node x _) <-
              runTree . runDiscardEffect $ runGenT size s0 . runTestT $ unPropertyT test
            case x of
              Nothing ->
                loop tests (discards + 1) (size + 1) s1

              Just (Left _, _) ->
                let
                  mkReport =
                    Report (tests + 1) discards
                in
                  fmap mkReport $
                    takeSmallest
                      size
                      seed
                      0
                      (propertyShrinkLimit cfg)
                      (propertyShrinkRetries cfg)
                      (updateUI . mkReport)
                      node

              Just (Right (), _) ->
                loop (tests + 1) discards (size + 1) s1
  in
    loop 0 0 size0 seed0

takeSmallest ::
     MonadIO m
  => Size
  -> Seed
  -> ShrinkCount
  -> ShrinkLimit
  -> ShrinkRetries
  -> (Progress -> m ())
  -> Node m (Maybe (Either Failure (), [Log]))
  -> m Result
takeSmallest size seed shrinks slimit retries updateUI = \case
  Node Nothing _ ->
    pure GaveUp

  Node (Just (x, w)) xs ->
    case x of
      Left (Failure loc err mdiff) -> do
        let
          failure =
            mkFailure size seed shrinks loc err mdiff (reverse w)

        updateUI $ Shrinking failure

        if shrinks >= fromIntegral slimit then
          -- if we've hit the shrink limit, don't shrink any further
          pure $ Failed failure
        else
          findM xs (Failed failure) $ \m -> do
            o <- runTreeN retries m
            if isFailure o then
              Just <$> takeSmallest size seed (shrinks + 1) slimit retries updateUI o
            else
              return Nothing

      Right () ->
        return OK


updateSummary :: Region -> TVar Summary -> Maybe UseColor -> (Summary -> Summary) -> IO ()
updateSummary sregion svar mcolor f = do
  summary <- atomically (TVar.modifyTVar' svar f >> TVar.readTVar svar)
  setRegion sregion =<< renderSummary mcolor summary

isFailure :: Node m (Maybe (Either x a, b)) -> Bool
isFailure = \case
  Node (Just (Left _, _)) _ ->
    True
  _ ->
    False

isSuccess :: Node m (Maybe (Either x a, b)) -> Bool
isSuccess =
  not . isFailure

runTreeN ::
     Monad m
  => ShrinkRetries
  -> Tree m (Maybe (Either x a, b))
  -> m (Node m (Maybe (Either x a, b)))
runTreeN n m = do
  o <- runTree m
  if n > 0 && isSuccess o then
    runTreeN (n - 1) m
  else
    pure o

findM :: Monad m => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] ->
      return def
    x0 : xs ->
      p x0 >>= \m ->
        case m of
          Nothing ->
            findM xs def p
          Just x ->
            return x

--  liftIO . displayRegion $ \region ->
--    (== OK) . reportStatus <$> checkNamed region Nothing Nothing prop

------------------------------------------------------------------------
-- Data

-- | A summary of all the properties executed.
--
data Summary =
  Summary {
      summaryWaiting :: !PropertyCount
    , summaryRunning :: !PropertyCount
    , summaryFailed :: !PropertyCount
    , summaryGaveUp :: !PropertyCount
    , summaryOK :: !PropertyCount
    } deriving (Show)

instance Monoid Summary where
  mempty =
    Summary 0 0 0 0 0
  mappend (Summary x1 x2 x3 x4 x5) (Summary y1 y2 y3 y4 y5) =
    Summary
      (x1 + y1)
      (x2 + y2)
      (x3 + y3)
      (x4 + y4)
      (x5 + y5)

instance Semigroup Summary where
  (<>) = mappend

-- | Construct a summary from a single result.
--
fromResult :: Result -> Summary
fromResult = \case
  Failed _ ->
    mempty { summaryFailed = 1 }
  GaveUp ->
    mempty { summaryGaveUp = 1 }
  OK ->
    mempty { summaryOK = 1 }

summaryCompleted :: Summary -> PropertyCount
summaryCompleted (Summary _ _ x3 x4 x5) =
  x3 + x4 + x5

summaryTotal :: Summary -> PropertyCount
summaryTotal (Summary x1 x2 x3 x4 x5) =
  x1 + x2 + x3 + x4 + x5

------------------------------------------------------------------------
-- Pretty Printing Helpers

data Line a =
  Line a LineNo String
    deriving (Eq, Ord, Show, Functor)

data Declaration a =
  Declaration {
      declarationFile :: !FilePath
    , declarationLine :: !LineNo
    , _declarationName :: !String
    , declarationSource :: !(Map LineNo (Line a))
    } deriving (Eq, Ord, Show, Functor)

data Style =
    StyleDefault
  | StyleAnnotation
  | StyleFailure
    deriving (Eq, Ord, Show)

data Markup =
    WaitingIcon
  | WaitingHeader
  | RunningIcon
  | RunningHeader
  | ShrinkingIcon
  | ShrinkingHeader
  | FailedIcon
  | FailedHeader
  | GaveUpIcon
  | GaveUpHeader
  | SuccessIcon
  | SuccessHeader
  | DeclarationLocation
  | StyledLineNo !Style
  | StyledBorder !Style
  | StyledSource !Style
  | AnnotationGutter
  | AnnotationValue
  | FailureArrows
  | FailureGutter
  | FailureMessage
  | DiffPrefix
  | DiffInfix
  | DiffSuffix
  | DiffSame
  | DiffRemoved
  | DiffAdded
  | ReproduceHeader
  | ReproduceGutter
  | ReproduceSource
    deriving (Eq, Ord, Show)

instance Semigroup Style where
  (<>) x y =
    case (x, y) of
      (StyleFailure, _) ->
        StyleFailure
      (_, StyleFailure) ->
        StyleFailure
      (StyleAnnotation, _) ->
        StyleAnnotation
      (_, StyleAnnotation) ->
        StyleAnnotation
      (StyleDefault, _) ->
        StyleDefault

------------------------------------------------------------------------

takeAnnotation :: Log -> Maybe FailedAnnotation
takeAnnotation = \case
  Annotation loc val ->
    Just $ FailedAnnotation loc val
  _ ->
    Nothing

takeFootnote :: Log -> Maybe String
takeFootnote = \case
  Footnote x ->
    Just x
  _ ->
    Nothing

mkFailure ::
     Size
  -> Seed
  -> ShrinkCount
  -> Maybe Span
  -> String
  -> Maybe Diff
  -> [Log]
  -> FailureReport
mkFailure size seed shrinks location message diff logs =
  let
    inputs =
      mapMaybe takeAnnotation logs

    footnotes =
      mapMaybe takeFootnote logs
  in
    FailureReport size seed shrinks inputs location message diff footnotes

------------------------------------------------------------------------
-- Pretty Printing

ppShow :: Show x => x -> Doc a
ppShow = -- unfortunate naming clash
  WL.text . show

markup :: Markup -> Doc Markup -> Doc Markup
markup =
  WL.annotate

icon :: Markup -> Char -> Doc Markup -> Doc Markup
icon m i x =
  markup m (WL.char i) <+> x

ppTestCount :: TestCount -> Doc a
ppTestCount = \case
  TestCount 1 ->
    "1 test"
  TestCount n ->
    ppShow n <+> "tests"

ppDiscardCount :: DiscardCount -> Doc a
ppDiscardCount = \case
  DiscardCount 1 ->
    "1 discard"
  DiscardCount n ->
    ppShow n <+> "discards"

ppShrinkCount :: ShrinkCount -> Doc a
ppShrinkCount = \case
  ShrinkCount 1 ->
    "1 shrink"
  ShrinkCount n ->
    ppShow n <+> "shrinks"

ppRawPropertyCount :: PropertyCount -> Doc a
ppRawPropertyCount (PropertyCount n) =
  ppShow n

ppWithDiscardCount :: DiscardCount -> Doc Markup
ppWithDiscardCount = \case
  DiscardCount 0 ->
    mempty
  n ->
    " with" <+> ppDiscardCount n

ppShrinkDiscard :: ShrinkCount -> DiscardCount -> Doc Markup
ppShrinkDiscard s d =
  case (s, d) of
    (0, 0) ->
      ""
    (0, _) ->
      " and" <+> ppDiscardCount d
    (_, 0) ->
      " and" <+> ppShrinkCount s
    (_, _) ->
      "," <+> ppShrinkCount s <+> "and" <+> ppDiscardCount d

mapSource :: (Map LineNo (Line a) -> Map LineNo (Line a)) -> Declaration a -> Declaration a
mapSource f decl =
  decl {
      declarationSource =
        f (declarationSource decl)
    }

-- | The span of non-whitespace characters for the line.
--
--   The result is @[inclusive, exclusive)@.
--
lineSpan :: Line a -> (ColumnNo, ColumnNo)
lineSpan (Line _ _ x0) =
  let
    (pre, x1) =
      span Char.isSpace x0

    (_, x2) =
      span Char.isSpace (reverse x1)

    start =
      length pre

    end =
      start + length x2
  in
    (fromIntegral start, fromIntegral end)

takeLines :: Span -> Declaration a -> Map LineNo (Line a)
takeLines sloc =
  fst . Map.split (spanEndLine sloc + 1) .
  snd . Map.split (spanStartLine sloc - 1) .
  declarationSource


checkFilePath :: FilePath -> IO Bool
checkFilePath path = isRight <$> (try $ readFile path :: IO (Either IOError String))

readDeclaration :: MonadIO m => Span -> m (Maybe (Declaration ()))
readDeclaration sloc =
  runMaybeT $ do
    path <- liftIO . makeRelativeToCurrentDirectory $ spanFile sloc
    exists <- liftIO $ checkFilePath path
    if not exists
      then mzero
      else do
       (name, Pos (Position _ line0 _) src) <- MaybeT $ Discovery.readDeclaration path (spanEndLine sloc)
       let line = fromIntegral line0
       pure . Declaration path line name .
         Map.fromList .
         zip [line..] .
         zipWith (Line ()) [line..] $
         lines src


defaultStyle :: Declaration a -> Declaration (Style, [(Style, Doc Markup)])
defaultStyle =
  fmap $ const (StyleDefault, [])

lastLineSpan :: Monad m => Span -> Declaration a -> MaybeT m (ColumnNo, ColumnNo)
lastLineSpan sloc decl =
  case reverse . Map.elems $ takeLines sloc decl of
    [] ->
      MaybeT $ pure Nothing
    x : _ ->
      pure $
        lineSpan x

ppFailedInputTypedArgument :: Int -> FailedAnnotation -> Doc Markup
ppFailedInputTypedArgument ix (FailedAnnotation _ val) =
  WL.vsep [
      WL.text "forAll" <> ppShow ix <+> "="
    , WL.indent 2 . WL.vsep . fmap (markup AnnotationValue . WL.text) $ lines val
    ]

ppFailedInputDeclaration ::
     MonadIO m
  => FailedAnnotation
  -> m (Maybe (Declaration (Style, [(Style, Doc Markup)])))
ppFailedInputDeclaration (FailedAnnotation msloc val) =
  runMaybeT $ do
    sloc <- MaybeT $ pure msloc
    decl <- fmap defaultStyle . MaybeT $ readDeclaration sloc
    startCol <- fromIntegral . fst <$> lastLineSpan sloc decl

    let
      ppValLine =
        WL.indent startCol .
          (markup AnnotationGutter (WL.text "│ ") <>) .
          markup AnnotationValue .
          WL.text

      valDocs =
        fmap ((StyleAnnotation, ) . ppValLine) $
        List.lines val

      startLine =
        fromIntegral $ spanStartLine sloc

      endLine =
        fromIntegral $ spanEndLine sloc

      styleInput kvs =
        foldr (Map.adjust . fmap . first $ const StyleAnnotation) kvs [startLine..endLine]

      insertDoc =
        Map.adjust (fmap . second $ const valDocs) endLine

    pure $
      mapSource (styleInput . insertDoc) decl

ppFailedInput ::
     MonadIO m
  => Int
  -> FailedAnnotation
  -> m (Either (Doc Markup) (Declaration (Style, [(Style, Doc Markup)])))
ppFailedInput ix input = do
  mdecl <- ppFailedInputDeclaration input
  case mdecl of
    Nothing ->
      pure . Left $ ppFailedInputTypedArgument ix input
    Just decl ->
      pure $ Right decl

ppLineDiff :: LineDiff -> Doc Markup
ppLineDiff = \case
  LineSame x ->
    markup DiffSame $
      "  " <> WL.text x

  LineRemoved x ->
    markup DiffRemoved $
      "- " <> WL.text x

  LineAdded x ->
    markup DiffAdded $
      "+ " <> WL.text x

ppDiff :: Diff -> [Doc Markup]
ppDiff (Diff prefix removed infix_ added suffix diff) = [
    markup DiffPrefix (WL.text prefix) <>
    markup DiffRemoved (WL.text removed) <+>
    markup DiffInfix (WL.text infix_) <+>
    markup DiffAdded (WL.text added) <>
    markup DiffSuffix (WL.text suffix)
  ] ++ fmap ppLineDiff (toLineDiff diff)

ppDeclaration :: Declaration (Style, [(Style, Doc Markup)]) -> Doc Markup
ppDeclaration decl =
  case Map.maxView $ declarationSource decl of
    Nothing ->
      mempty
    Just _ ->
      let
        ppLines = do
          Line (_, xs) _ _ <- Map.elems $ declarationSource decl
          fmap snd xs
      in
        WL.vsep (ppLines)

mergeLine :: Semigroup a => Line a -> Line a -> Line a
mergeLine (Line x no src) (Line y _ _) =
  Line (x <> y) no src

mergeDeclaration :: Semigroup a => Declaration a -> Declaration a -> Declaration a
mergeDeclaration (Declaration file line name src0) (Declaration _ _ _ src1) =
  Declaration file line name $
  Map.unionWith mergeLine src0 src1

mergeDeclarations :: Semigroup a => [Declaration a] -> [Declaration a]
mergeDeclarations =
  Map.elems .
  Map.fromListWith mergeDeclaration .
  fmap (\d -> ((declarationFile d, declarationLine d), d))

ppTextLines :: String -> [Doc Markup]
ppTextLines =
  fmap WL.text . List.lines

ppFailureReport :: MonadIO m => Maybe PropertyName -> FailureReport -> m (Doc Markup)
ppFailureReport _ (FailureReport _ _ _ inputs0 _ msg mdiff msgs0) = do
  _ <- let
          msgs1 =
            msgs0 ++
            (if null msg then [] else [msg])

          docs =
            concatMap ppTextLines msgs1 ++
            maybe [] ppDiff mdiff
        in
          pure (docs, Nothing)

  (_, idecls) <- partitionEithers <$> zipWithM ppFailedInput [0..] inputs0

  let
    decls =
      mergeDeclarations .
      catMaybes $
        fmap pure idecls

    with xs f =
      if null xs then
        []
      else
        [f xs]

  pure . WL.indent 2 . WL.vsep . WL.punctuate WL.line $ concat [
      with decls $
        WL.vsep . WL.punctuate WL.line . fmap ppDeclaration
    ]

ppName :: Maybe PropertyName -> Doc a
ppName = \case
  Nothing ->
    "<interactive>"
  Just (PropertyName name) ->
    WL.text name

ppProgress :: MonadIO m => Maybe PropertyName -> Report Progress -> m (Doc Markup)
ppProgress name (Report tests discards status) =
  case status of
    Running ->
      pure . icon RunningIcon '●' . WL.annotate RunningHeader $
        ppName name <+>
        "passed" <+>
        ppTestCount tests <>
        ppWithDiscardCount discards <+>
        "(running)"

    Shrinking failure ->
      pure . icon ShrinkingIcon '↯' . WL.annotate ShrinkingHeader $
        ppName name <+>
        "failed after" <+>
        ppTestCount tests <>
        ppShrinkDiscard (failureShrinks failure) discards <+>
        "(shrinking)"

ppResult :: MonadIO m => Maybe PropertyName -> Report Result -> m (Doc Markup)
ppResult name (Report tests discards result) =
  case result of
    Failed failure -> do
      pfailure <- ppFailureReport name failure
      pure . WL.vsep $ [
          icon FailedIcon '✗' . WL.annotate FailedHeader $
            ppName name <+>
            "failed after" <+>
            ppTestCount tests <>
            ppShrinkDiscard (failureShrinks failure) discards <>
            "."
        , mempty
        , pfailure
        , mempty
        ]

    GaveUp ->
      pure . icon GaveUpIcon '⚐' . WL.annotate GaveUpHeader $
        ppName name <+>
        "gave up after" <+>
        ppDiscardCount discards <>
        ", passed" <+>
        ppTestCount tests <>
        "."

    OK ->
      pure . icon SuccessIcon '✓' . WL.annotate SuccessHeader $
        ppName name <+>
        "passed" <+>
        ppTestCount tests <>
        "."

ppWhenNonZero :: Doc a -> PropertyCount -> Maybe (Doc a)
ppWhenNonZero suffix n =
  if n <= 0 then
    Nothing
  else
    Just $ ppRawPropertyCount n <+> suffix

annotateSummary :: Summary -> Doc Markup -> Doc Markup
annotateSummary summary =
  if summaryFailed summary > 0 then
    icon FailedIcon '✗' . WL.annotate FailedHeader
  else if summaryGaveUp summary > 0 then
    icon GaveUpIcon '⚐' . WL.annotate GaveUpHeader
  else if summaryWaiting summary > 0 || summaryRunning summary > 0 then
    icon WaitingIcon '○' . WL.annotate WaitingHeader
  else
    icon SuccessIcon '✓' . WL.annotate SuccessHeader

ppSummary :: MonadIO m => Summary -> m (Doc Markup)
ppSummary summary =
  let
    complete =
      summaryCompleted summary == summaryTotal summary

    prefix end =
      if complete then
        mempty
      else
        ppRawPropertyCount (summaryCompleted summary) <>
        "/" <>
        ppRawPropertyCount (summaryTotal summary) <+>
        "complete" <> end

    addPrefix xs =
      if null xs then
        prefix mempty : []
      else
        prefix ": " : xs

    suffix =
      if complete then
        "."
      else
        " (running)"
  in
    pure .
      annotateSummary summary .
      (<> suffix) .
      WL.hcat .
      addPrefix .
      WL.punctuate ", " $
      catMaybes [
          ppWhenNonZero "failed" (summaryFailed summary)
        , ppWhenNonZero "gave up" (summaryGaveUp summary)
        , if complete then
            ppWhenNonZero "succeeded" (summaryOK summary)
          else
            Nothing
        ]

renderDoc :: MonadIO m => Maybe UseColor -> Doc Markup -> m String
renderDoc mcolor doc = do
  let
    dull =
      SetColor Foreground Dull

    vivid =
      SetColor Foreground Vivid

    bold =
      SetConsoleIntensity BoldIntensity

    start = \case
      WaitingIcon ->
        setSGRCode []
      WaitingHeader ->
        setSGRCode []
      RunningIcon ->
        setSGRCode []
      RunningHeader ->
        setSGRCode []
      ShrinkingIcon ->
        setSGRCode [vivid Red]
      ShrinkingHeader ->
        setSGRCode [vivid Red]
      FailedIcon ->
        setSGRCode [vivid Red]
      FailedHeader ->
        setSGRCode [vivid Red]
      GaveUpIcon ->
        setSGRCode [dull Yellow]
      GaveUpHeader ->
        setSGRCode [dull Yellow]
      SuccessIcon ->
        setSGRCode [dull Green]
      SuccessHeader ->
        setSGRCode [dull Green]

      DeclarationLocation ->
        setSGRCode []

      StyledLineNo StyleDefault ->
        setSGRCode []
      StyledSource StyleDefault ->
        setSGRCode []
      StyledBorder StyleDefault ->
        setSGRCode []

      StyledLineNo StyleAnnotation ->
        setSGRCode [dull Magenta]
      StyledSource StyleAnnotation ->
        setSGRCode []
      StyledBorder StyleAnnotation ->
        setSGRCode []
      AnnotationGutter ->
        setSGRCode [dull Magenta]
      AnnotationValue ->
        setSGRCode [dull Magenta]

      StyledLineNo StyleFailure ->
        setSGRCode [vivid Red]
      StyledSource StyleFailure ->
        setSGRCode [vivid Red, bold]
      StyledBorder StyleFailure ->
        setSGRCode []
      FailureArrows ->
        setSGRCode [vivid Red]
      FailureMessage ->
        setSGRCode []
      FailureGutter ->
        setSGRCode []

      DiffPrefix ->
        setSGRCode []
      DiffInfix ->
        setSGRCode []
      DiffSuffix ->
        setSGRCode []
      DiffSame ->
        setSGRCode []
      DiffRemoved ->
        setSGRCode [dull Red]
      DiffAdded ->
        setSGRCode [dull Green]

      ReproduceHeader ->
        setSGRCode []
      ReproduceGutter ->
        setSGRCode []
      ReproduceSource ->
        setSGRCode []

    end _ =
      setSGRCode [Reset]

  color <- resolveColor mcolor

  let
    display =
      case color of
        EnableColor ->
          WL.displayDecorated start end id
        DisableColor ->
          WL.display

  pure .
    display .
    WL.renderSmart 100 $
    WL.indent 2 doc

renderProgress :: MonadIO m => Maybe UseColor -> Maybe PropertyName -> Report Progress -> m String
renderProgress mcolor name x =
  renderDoc mcolor =<< ppProgress name x

renderResult :: MonadIO m => Maybe UseColor -> Maybe PropertyName -> Report Result -> m String
renderResult mcolor name x =
  renderDoc mcolor =<< ppResult name x

renderSummary :: MonadIO m => Maybe UseColor -> Summary -> m String
renderSummary mcolor x =
  renderDoc mcolor =<< ppSummary x
