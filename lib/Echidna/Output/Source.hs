{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Output.Source where

import Control.Monad (unless)
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List (intercalate, nub, sort, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, catMaybes)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import HTMLEntities.Text qualified as HTML
import Prelude hiding (writeFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), splitDirectories, joinPath, takeDirectory)
import System.FilePath.Glob qualified as Glob
import Text.Mustache (substituteValue, toMustache)
import Text.Mustache.Compile (embedTemplate)
import Language.Haskell.TH.Syntax (addDependentFile)
import Text.Mustache.Types (Template, Value(..))
import Text.Printf (printf)

import EVM.Dapp (srcMapCodePos, DappInfo(..))
import EVM.Solidity (CodeType(..), SourceCache(..), SrcMap, SolcContract(..))
import EVM.Types (W256)

import Echidna.SourceAnalysis.Slither (AssertLocation(..), assertLocationList, SlitherInfo(..))
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Coverage.HitCounts (HitCountSnapshot(..), snapshotHitCounts)
import Echidna.Types.Coverage
  (CoverageInfo, OpIx, TxResults, unpackTxResults, CoverageFileType (..), snapshotUnits)
import Echidna.Types.Tx (TxResult(..))

-- | Embedded template with partials for coverage reports. The templates are
-- registered as dependencies so editing them recompiles this module; the
-- embedding alone does not.
coverageTemplate :: Template
coverageTemplate = $(do
  mapM_ addDependentFile
    [ "lib/Echidna/Output/assets/coverage.mustache"
    , "lib/Echidna/Output/assets/styles.mustache"
    , "lib/Echidna/Output/assets/scripts.mustache" ]
  embedTemplate ["lib/Echidna/Output/assets"] "coverage.mustache")

-- | Filter files based on exclude patterns, using relative paths from common prefix
filterExcludedFiles :: [Text] -> FilePath -> [(FilePath, V.Vector Text)] -> [(FilePath, V.Vector Text)]
filterExcludedFiles excludePatterns commonPrefix allFiles =
  let patterns = map (Glob.compile . T.unpack) excludePatterns
      isExcluded filePath =
        let relativePath = makeRelativePath commonPrefix filePath
        in any (`Glob.match` relativePath) patterns
  in filter (not . isExcluded . fst) allFiles

saveCoverages
  :: Env
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> IO ()
saveCoverages env seed d sc cs = do
  let fileTypes = env.cfg.campaignConf.coverageFormats
      coverageExcludes = env.cfg.campaignConf.coverageExcludes
      projectName = env.cfg.projectName
  units <- snapshotUnits env.coverageRefInit env.coverageRefRuntime
  -- Hit counts are reported only when the campaign kept them.
  hits <- if env.hitCountBudget > 0 then Just <$> snapshotHitCounts env.coverageSlots else pure Nothing
  mapM_ (\ty -> saveCoverage ty seed d sc cs units hits projectName coverageExcludes) fileTypes

saveCoverage
  :: CoverageFileType
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> UnitCoverage
  -> Maybe HitCounts
  -> Maybe Text
  -> [Text]
  -> IO ()
saveCoverage fileType seed d sc cs units hits projectName excludePatterns = do
  let extension = coverageFileExtension fileType
      fn = d </> "covered." <> show seed <> extension
  currentTime <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%B %d, %Y at %H:%M:%S UTC" currentTime
      cc = ppCoveredCode fileType sc cs units hits projectName timestamp excludePatterns
  createDirectoryIfMissing True d
  writeFile fn cc

coverageFileExtension :: CoverageFileType -> String
coverageFileExtension Lcov = ".lcov"
coverageFileExtension Html = ".html"
coverageFileExtension Txt = ".txt"

-- | Per-pc coverage of every code unit, keyed by unit.
type UnitCoverage = Map (CodeType, W256) (VU.Vector CoverageInfo)

-- | Committed hit counts of every code unit, keyed by unit.
type HitCounts = Map (CodeType, W256) HitCountSnapshot

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> UnitCoverage -> Maybe HitCounts -> Maybe Text -> Text -> [Text] -> Text
ppCoveredCode fileType sc cs units hits projectName timestamp excludePatterns
  | null units = "Coverage map is empty"
  | Html <- fileType = htmlTemplate filteredFiles runtimeLinesMap covLines showCounts projectName timestamp commonPrefix
  | otherwise = let
    -- Pretty print individual file coverage
    ppFile (srcPath, srcLines) =
      let runtimeLines = fromMaybe mempty $ Map.lookup srcPath runtimeLinesMap
          marked = markLines fileType showCounts srcLines runtimeLines (fromMaybe Map.empty (Map.lookup srcPath covLines))
      in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
    -- Text to add to top of the file
    topHeader = case fileType of
      Lcov -> "TN:\n"
      Txt | showCounts -> "line | result markers | executions | failed executions | source\n"
          | otherwise -> ""
    -- Alter file name
    changeFileName (T.pack -> fn) = case fileType of
      Lcov -> "SF:" <> fn
      Txt  -> fn
    -- Alter file contents
    changeFileLines ls = case fileType of
      Lcov -> ls ++ ["end_of_record"]
      Txt  -> ls
    in topHeader <> T.unlines (map ppFile filteredFiles)
  where
    -- Count columns appear when the campaign kept hit counts
    showCounts = isJust hits
    -- List of covered lines during the fuzzing campaign
    covLines = srcMapCov sc units hits cs
    -- Collect all the possible lines from all the files
    allFiles = (\(path, src) -> (path, V.fromList (decodeUtf8 <$> BS.split 0xa src))) <$> Map.elems sc.files
    -- Find common path prefix for filtering
    commonPrefix = findCommonPathPrefix (map fst allFiles)
    -- Filter out excluded files using relative paths
    filteredFiles = filterExcludedFiles excludePatterns commonPrefix allFiles
    -- Excludes lines such as comments or blanks
    runtimeLinesMap = buildRuntimeLinesMap sc cs

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: CoverageFileType -> Bool -> V.Vector Text -> S.Set Int -> Map Int LineCoverage -> V.Vector Text
markLines fileType showCounts codeLines runtimeLines lineMap =
  V.map markLine . V.filter shouldUseLine $ V.indexed codeLines
  where
  shouldUseLine (i, _) = case fileType of
    Lcov -> i + 1 `elem` runtimeLines
    _ -> True
  markLine (i, codeLine) =
    let n = i + 1
        covered = Map.lookup n lineMap
        results = maybe [] (unpackTxResults . (.results)) covered
        markers = sort $ nub $ getMarker <$> results
        -- Counts: blank on lines that never ran, '?' where a unit mapping
        -- here keeps no counts
        (execsCol, failedCol) = case covered of
          Nothing -> ("", "")
          Just lc -> case lc.counts of
            Just c -> (show c.execs, show c.failedExecs)
            Nothing -> ("?", "?")
        wrapLine :: Text -> Text
        wrapLine line = case fileType of
          Html -> "<span class='" <> cssClass <> "'>" <>
                        HTML.text line <>
                     "</span>"
          _ -> line
          where
          cssClass = if n `elem` runtimeLines then getCSSClass markers else "n" -- fallback to 'neutral' class.
        -- LCOV takes an execution count; without one (counts off, or unknown
        -- for this line) fall back to the boolean 1/0 form
        lcovCount = case covered of
          Just LineCoverage { counts = Just c } | showCounts -> c.execs
          Just _ -> 1 :: Int
          Nothing -> 0
        result = case fileType of
          Lcov -> pack $ printf "DA:%d,%d" n lcovCount
          _ | showCounts -> pack $ printf " %*d | %-4s| %10s | %10s | %s" lineNrSpan n markers execsCol failedCol (wrapLine codeLine)
            | otherwise -> pack $ printf " %*d | %-4s| %s" lineNrSpan n markers (wrapLine codeLine)

    in result
  lineNrSpan = length . show $ V.length codeLines + 1

getCSSClass :: String -> Text
getCSSClass markers =
  case markers of
   []                      -> "u" -- unexecuted
   _  | '*' `elem` markers -> "e" -- executed
   _                       -> "r" -- reverted

-- | Select the proper marker, according to the result of the transaction
getMarker :: TxResult -> Char
getMarker ReturnTrue    = '*'
getMarker ReturnFalse   = '*'
getMarker Stop          = '*'
getMarker ErrorRevert   = 'r'
getMarker ErrorOutOfGas = 'o'
getMarker _             = 'e'

-- | Coverage of one source line.
data LineCoverage = LineCoverage
  { results :: TxResults
    -- ^ transaction results observed with this line's instructions last
  , counts :: Maybe Counts
    -- ^ execution counts; 'Nothing' when unknown, because a unit mapping to
    -- this line keeps no counts (too large, over budget, or dropped)
  }

data Counts = Counts
  { execs :: Int
    -- ^ executions of the line's instructions inside completed transactions
  , failedExecs :: Int
    -- ^ those inside transactions that did not succeed
  }

-- | Merge across distinct code units: results accumulate, counts add up, and
-- an unknown count is absorbing.
instance Semigroup LineCoverage where
  a <> b = LineCoverage (a.results .|. b.results) (liftA2 addCounts a.counts b.counts)
    where addCounts x y = Counts (x.execs + y.execs) (x.failedExecs + y.failedExecs)

-- | Merge within one code unit, where several instructions (and several
-- owning contracts' source maps) land on the same line: the line ran as often
-- as its most-executed instruction, never the sum.
maxLine :: LineCoverage -> LineCoverage -> LineCoverage
maxLine a b = LineCoverage (a.results .|. b.results) (liftA2 maxCounts a.counts b.counts)
  where maxCounts x y = Counts (max x.execs y.execs) (max x.failedExecs y.failedExecs)

type LineMap = Map FilePath (Map Int LineCoverage)

-- | Source lines covered, with hit counts when available. Code units are the
-- runtime and creation code of every contract, each processed once even when
-- several contracts share it; a shared unit is attributed to each owner's
-- source map with the within-unit merge, and only distinct units are summed.
srcMapCov :: SourceCache -> UnitCoverage -> Maybe HitCounts -> [SolcContract] -> LineMap
srcMapCov sc units hits contracts =
  Map.unionsWith (Map.unionWith (<>)) [unitLines unit owners | (unit, owners) <- Map.toList unitOwners]
  where
  unitOwners :: Map (CodeType, W256) [SolcContract]
  unitOwners = Map.fromListWith (++) $ concat
    [ [((Runtime, c.runtimeCodehash), [c]), ((Creation, c.creationCodehash), [c])] | c <- contracts ]

  unitLines unit@(kind, _) owners = case Map.lookup unit units of
    Nothing -> mempty
    Just vec -> Map.unionsWith (Map.unionWith maxLine)
      [ linesOf c kind vec (countsAt unit) | c <- sortOn (.contractName) owners ]

  -- Per-pc counts of a unit, if the campaign kept them for it
  countsAt unit = case hits >>= Map.lookup unit of
    Just snap | not snap.incomplete -> \pc -> Just (Counts (snap.execs VU.! pc) (snap.failedExecs VU.! pc))
    _ -> const Nothing

  linesOf :: SolcContract -> CodeType -> VU.Vector CoverageInfo -> (Int -> Maybe Counts) -> LineMap
  linesOf c kind vec countAt = VU.ifoldl' step mempty vec
    where
    -- creation-code op indices follow the runtime source map (see srcMapForOpLocation)
    offset = case kind of
      Runtime -> 0
      Creation -> length c.runtimeSrcmap
    step acc pc (opIx, _stackDepths, txResults)
      | opIx == -1 = acc -- not covered
      | otherwise = case srcMapForOpLocation c (opIx + offset) >>= srcMapCodePos sc of
          Just (file, line) ->
            Map.insertWith (Map.unionWith maxLine) file
              (Map.singleton line (LineCoverage txResults (countAt pc))) acc
          Nothing -> acc

-- | Given a contract, and tuple as coverage, return the corresponding mapped line (if any)
srcMapForOpLocation :: SolcContract -> OpIx -> Maybe SrcMap
srcMapForOpLocation contract opIx =
  Seq.lookup opIx (contract.runtimeSrcmap <> contract.creationSrcmap)

-- | Builds a Map from file paths to lines that can be executed, this excludes
-- for example lines with comments
buildRuntimeLinesMap :: SourceCache -> [SolcContract] -> Map FilePath (S.Set Int)
buildRuntimeLinesMap sc contracts =
  Map.fromListWith (<>)
    [(k, S.singleton v) | (k, v) <- mapMaybe (srcMapCodePos sc) srcMaps]
  where
  srcMaps = concatMap
    (\c -> toList $ c.runtimeSrcmap <> c.creationSrcmap) contracts

-- | Check that all assertions were hit, and log a warning if they weren't
checkAssertionsCoverage
  :: SourceCache
  -> Env
  -> IO ()
checkAssertionsCoverage sc env = do
  units <- snapshotUnits env.coverageRefInit env.coverageRefRuntime
  let
    cs = Map.elems env.dapp.solcByName
    asserts = maybe [] (concatMap assertLocationList . Map.elems . (.asserts)) env.slitherInfo
    covLines = srcMapCov sc units Nothing cs
  mapM_ (checkAssertionReached covLines) asserts

-- | Helper function for `checkAssertionsCoverage` which checks a single assertion
-- and logs a warning if it wasn't hit
checkAssertionReached :: LineMap -> AssertLocation -> IO ()
checkAssertionReached covLines assert =
  maybe
    warnAssertNotReached checkCoverage
    (Map.lookup assert.filenameAbsolute covLines)
  where
   checkCoverage coverage = let lineNumbers = Map.keys coverage in
     unless (NE.head assert.assertLines `elem` lineNumbers) warnAssertNotReached
   warnAssertNotReached =
    putStrLn $ "WARNING: assertion at file: " ++ assert.filenameRelative
       ++ " starting at line: " ++ show (NE.head assert.assertLines) ++ " was never reached"

-- | Find the common path prefix among all file paths
findCommonPathPrefix :: [FilePath] -> FilePath
findCommonPathPrefix [] = ""
findCommonPathPrefix [path] = takeDirectory path
findCommonPathPrefix paths =
  let pathComponents = map splitDirectories paths
      commonComponents = foldl1 commonPrefix pathComponents
  in joinPath commonComponents
  where
    commonPrefix [] _ = []
    commonPrefix _ [] = []
    commonPrefix (x:xs) (y:ys)
      | x == y = x : commonPrefix xs ys
      | otherwise = []

-- | Convert absolute path to relative path given a base directory
makeRelativePath :: FilePath -> FilePath -> FilePath
makeRelativePath basePath filePath =
  let baseComponents = splitDirectories basePath
      fileComponents = splitDirectories filePath
  in maybe filePath joinPath (stripPrefix baseComponents fileComponents)
  where
    stripPrefix [] ys = Just ys
    stripPrefix _ [] = Nothing
    stripPrefix (x:xs) (y:ys)
      | x == y = stripPrefix xs ys
      | otherwise = Nothing

-- | Generate modern HTML coverage report using mustache template
htmlTemplate :: [(FilePath, V.Vector Text)] -> Map FilePath (S.Set Int) -> LineMap -> Bool -> Maybe Text -> Text -> FilePath -> Text
htmlTemplate allFiles runtimeLinesMap covLines showCounts projectName timestamp commonPrefix =
  substituteValue coverageTemplate $ buildTemplateContext allFiles runtimeLinesMap covLines showCounts projectName timestamp commonPrefix

-- | Build the context object for the mustache template
buildTemplateContext :: [(FilePath, V.Vector Text)] -> Map FilePath (S.Set Int) -> LineMap -> Bool -> Maybe Text -> Text -> FilePath -> Value
buildTemplateContext allFiles runtimeLinesMap covLines showCounts projectName timestamp commonPrefix =
  let
    totalFiles = length allFiles
    (totalLines, totalCoveredLines, totalActiveLines) = calculateTotalStats allFiles runtimeLinesMap covLines
    coveragePercentage = if totalActiveLines == 0 then 0 else (totalCoveredLines * 100) `div` totalActiveLines

    -- Build title with optional project name
    title = case projectName of
      Just name -> "Echidna Coverage Report - " <> name
      Nothing -> "Echidna Coverage Report"

    filesData = map (buildFileContext runtimeLinesMap covLines commonPrefix) allFiles
  in toMustache $ (Map.fromList :: [(Text, Value)] -> Map Text Value)
    [ ("title", toMustache title)
    , ("totalFiles", toMustache $ T.pack $ show totalFiles)
    , ("totalLines", toMustache $ T.pack $ show totalLines)
    , ("totalCoveredLines", toMustache $ T.pack $ show totalCoveredLines)
    , ("totalActiveLines", toMustache $ T.pack $ show totalActiveLines)
    , ("coveragePercentage", toMustache $ T.pack $ printf "%.1f" (fromIntegral coveragePercentage :: Double))
    , ("coverageColor", toMustache $ getCoverageColorHsl coveragePercentage)
    , ("timestamp", toMustache timestamp)
    , ("showCounts", toMustache showCounts)
    , ("files", toMustache filesData)
    ]

-- | Build context for a single file
buildFileContext :: Map FilePath (S.Set Int) -> LineMap -> FilePath -> (FilePath, V.Vector Text) -> Value
buildFileContext runtimeLinesMap covLines commonPrefix (srcPath, srcLines) =
  let
    runtimeLines = fromMaybe mempty $ Map.lookup srcPath runtimeLinesMap
    covered = fromMaybe Map.empty (Map.lookup srcPath covLines)
    activeLines = S.size runtimeLines
    coveredLines = length $ filter (`Map.member` covered) (S.toList runtimeLines)
    coveragePercentage = if activeLines == 0 then 0 else (coveredLines * 100) `div` activeLines

    -- Use relative path for display
    displayPath = makeRelativePath commonPrefix srcPath
    fileId = T.pack $ map (\c -> if c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" :: String) then c else '_') displayPath

    linesData = V.toList $ V.imap (buildLineContext runtimeLines covered) srcLines
  in toMustache $ (Map.fromList :: [(Text, Value)] -> Map Text Value)
    [ ("fileId", toMustache fileId)
    , ("filePath", toMustache $ T.pack displayPath)
    , ("activeLines", toMustache $ T.pack $ show activeLines)
    , ("coveredLines", toMustache $ T.pack $ show coveredLines)
    , ("coveragePercentage", toMustache $ T.pack $ printf "%.1f" (fromIntegral coveragePercentage :: Double))
    , ("coverageColor", toMustache $ getCoverageColorHsl coveragePercentage)
    , ("coverageColorAlpha", toMustache $ getCoverageColorAlpha coveragePercentage)
    , ("lines", toMustache linesData)
    ]

-- | Build context for a single line of code
buildLineContext :: S.Set Int -> Map Int LineCoverage -> Int -> Text -> Value
buildLineContext runtimeLines covered lineIndex codeLine =
  let
    lineNum = lineIndex + 1
    lineCov = Map.lookup lineNum covered
    isActive = lineNum `S.member` runtimeLines
    isCovered = isJust lineCov
    -- Count cells, medusa style: executions in successful transactions and in
    -- failed ones, compacted (1.2K) with the exact figure in the tooltip; the
    -- text report keeps full precision
    counts = lineCov >>= (.counts)
    successCount = maybe 0 (\c -> c.execs - c.failedExecs) counts
    failedCount = maybe 0 (.failedExecs) counts
    countsUnknown = isCovered && isNothing counts
    successTitle = "Executed " <> withCommas successCount <> " times in successful transactions"
    failedTitle = "Executed " <> withCommas failedCount <> " times in failed transactions (revert, out of gas, error)"
    rowClass
      | not isActive = Nothing
      | isCovered = Just ("row-line-covered" :: Text)
      | otherwise = Just ("row-line-uncovered" :: Text)

  in toMustache $ (Map.fromList :: [(Text, Value)] -> Map Text Value) $ catMaybes
    [ Just ("lineNumber", toMustache $ T.pack $ show lineNum)
    , Just ("sourceCode", toMustache codeLine)
    , Just ("hasSuccess", toMustache (successCount > 0))
    , Just ("successCount", toMustache (compactCount successCount))
    , Just ("successTitle", toMustache successTitle)
    , Just ("hasFailed", toMustache (failedCount > 0))
    , Just ("failedCount", toMustache (compactCount failedCount))
    , Just ("failedTitle", toMustache failedTitle)
    , Just ("countsUnknown", toMustache countsUnknown)
    , fmap (\v -> ("rowClass", toMustache v)) rowClass
    ]

-- | A count fit for a narrow cell: 999, 1.2K, 3.4M, 5.6B.
compactCount :: Int -> Text
compactCount n
  | n < 1000 = T.pack (show n)
  | n < 1000000 = scaled 1000 "K"
  | n < 1000000000 = scaled 1000000 "M"
  | otherwise = scaled 1000000000 "B"
  where
    scaled :: Int -> Text -> Text
    scaled d unit = T.pack (printf "%.1f" (fromIntegral n / fromIntegral d :: Double)) <> unit

-- | A count with thousands separators, for tooltips.
withCommas :: Int -> Text
withCommas n = T.pack $ reverse $ intercalate "," $ chunksOf3 $ reverse (show n)
  where
    chunksOf3 [] = []
    chunksOf3 xs = let (a, b) = splitAt 3 xs in a : chunksOf3 b

-- | Calculate total statistics across all files
calculateTotalStats :: [(FilePath, V.Vector Text)] -> Map FilePath (S.Set Int) -> LineMap -> (Int, Int, Int)
calculateTotalStats allFiles runtimeLinesMap covLines =
  let
    fileStats (srcPath, srcLines) =
      let runtimeLines = fromMaybe mempty $ Map.lookup srcPath runtimeLinesMap
          covered = fromMaybe Map.empty (Map.lookup srcPath covLines)
          fileTotalLines = V.length srcLines
          activeLines = S.size runtimeLines
          coveredLines = length $ filter (`Map.member` covered) (S.toList runtimeLines)
      in (fileTotalLines, coveredLines, activeLines)

    allStats = map fileStats allFiles
    totalLines = sum $ map (\(t,_,_) -> t) allStats
    totalCoveredLines = sum $ map (\(_,c,_) -> c) allStats
    totalActiveLines = sum $ map (\(_,_,a) -> a) allStats
  in (totalLines, totalCoveredLines, totalActiveLines)

-- | Get HSL color based on coverage percentage
getCoverageColorHsl :: Int -> Text
getCoverageColorHsl percentage
  | percentage < 50 = "hsl(" <> T.pack (show (percentage * 12 `div` 10)) <> ", 90%, 50%)"
  | otherwise = "hsl(" <> T.pack (show (60 + ((percentage - 50) * 12 `div` 10))) <> ", 90%, 45%)"

-- | Get HSL color with alpha based on coverage percentage
getCoverageColorAlpha :: Int -> Text
getCoverageColorAlpha percentage
  | percentage < 50 = "hsla(" <> T.pack (show (percentage * 12 `div` 10)) <> ", 90%, 50%, 0.15)"
  | otherwise = "hsla(" <> T.pack (show (60 + ((percentage - 50) * 12 `div` 10))) <> ", 90%, 45%, 0.15)"
