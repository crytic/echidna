{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Text.Mustache (substituteValue, toMustache)
import Text.Mustache.Compile (embedTemplate)
import Text.Mustache.Types (Template, Value(..))
import Data.Foldable
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Map (Map)
import Data.Map qualified as Map
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
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), splitDirectories, joinPath, takeDirectory)
import System.FilePath.Glob qualified as Glob
import Text.Printf (printf)

import EVM.Dapp (srcMapCodePos, DappInfo(..))
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Coverage (OpIx, unpackTxResults, FrozenCoverageMap, CoverageFileType (..), mergeCoverageMaps)
import Echidna.Types.Tx (TxResult(..))
import Echidna.SourceAnalysis.Slither (AssertLocation(..), assertLocationList, SlitherInfo(..))

-- | Embedded template with partials for coverage reports
coverageTemplate :: Template
coverageTemplate = $(embedTemplate ["lib/Echidna/Output/assets"] "coverage.mustache")

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
  coverage <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  mapM_ (\ty -> saveCoverage ty seed d sc cs coverage projectName coverageExcludes) fileTypes

saveCoverage
  :: CoverageFileType
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> FrozenCoverageMap
  -> Maybe Text
  -> [Text]
  -> IO ()
saveCoverage fileType seed d sc cs covMap projectName excludePatterns = do
  let extension = coverageFileExtension fileType
      fn = d </> "covered." <> show seed <> extension
  currentTime <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%B %d, %Y at %H:%M:%S UTC" currentTime
      cc = ppCoveredCode fileType sc cs covMap projectName timestamp excludePatterns
  createDirectoryIfMissing True d
  writeFile fn cc

coverageFileExtension :: CoverageFileType -> String
coverageFileExtension Lcov = ".lcov"
coverageFileExtension Html = ".html"
coverageFileExtension Txt = ".txt"

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> FrozenCoverageMap -> Maybe Text -> Text -> [Text] -> Text
ppCoveredCode fileType sc cs s projectName timestamp excludePatterns
  | null s = "Coverage map is empty"
  | Html <- fileType = htmlTemplate filteredFiles runtimeLinesMap covLines projectName timestamp commonPrefix
  | otherwise = let
    -- Pretty print individual file coverage
    ppFile (srcPath, srcLines) =
      let runtimeLines = fromMaybe mempty $ Map.lookup srcPath runtimeLinesMap
          marked = markLines fileType srcLines runtimeLines (fromMaybe Map.empty (Map.lookup srcPath covLines))
      in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
    -- Text to add to top of the file
    topHeader = case fileType of
      Lcov -> "TN:\n"
      Txt  -> ""
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
    -- List of covered lines during the fuzzing campaign
    covLines = srcMapCov sc s cs
    -- Collect all the possible lines from all the files
    allFiles = (\(path, src) -> (path, V.fromList (decodeUtf8 <$> BS.split 0xa src))) <$> Map.elems sc.files
    -- Find common path prefix for filtering
    commonPrefix = findCommonPathPrefix (map fst allFiles)
    -- Filter out excluded files using relative paths
    filteredFiles = filterExcludedFiles excludePatterns commonPrefix allFiles
    -- Excludes lines such as comments or blanks
    runtimeLinesMap = buildRuntimeLinesMap sc cs

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: CoverageFileType -> V.Vector Text -> S.Set Int -> Map Int [TxResult] -> V.Vector Text
markLines fileType codeLines runtimeLines resultMap =
  V.map markLine . V.filter shouldUseLine $ V.indexed codeLines
  where
  shouldUseLine (i, _) = case fileType of
    Lcov -> i + 1 `elem` runtimeLines
    _ -> True
  markLine (i, codeLine) =
    let n = i + 1
        results  = fromMaybe [] (Map.lookup n resultMap)
        markers = sort $ nub $ getMarker <$> results
        wrapLine :: Text -> Text
        wrapLine line = case fileType of
          Html -> "<span class='" <> cssClass <> "'>" <>
                        HTML.text line <>
                     "</span>"
          _ -> line
          where
          cssClass = if n `elem` runtimeLines then getCSSClass markers else "n" -- fallback to 'neutral' class.
        result = case fileType of
          Lcov -> pack $ printf "DA:%d,%d" n (length results)
          _ -> pack $ printf " %*d | %-4s| %s" lineNrSpan n markers (wrapLine codeLine)

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

-- | Given a source cache, a coverage map, a contract returns a list of covered lines
srcMapCov :: SourceCache -> FrozenCoverageMap -> [SolcContract] -> Map FilePath (Map Int [TxResult])
srcMapCov sc covMap contracts =
  Map.unionsWith Map.union $ linesCovered <$> contracts
  where
  linesCovered :: SolcContract -> Map FilePath (Map Int [TxResult])
  linesCovered c =
    case Map.lookup c.runtimeCodehash covMap of
      Just vec -> VU.foldl' (\acc covInfo -> case covInfo of
        (-1, _, _) -> acc -- not covered
        (opIx, _stackDepths, txResults) ->
          case srcMapForOpLocation c opIx of
            Just srcMap ->
              case srcMapCodePos sc srcMap of
                Just (file, line) ->
                  Map.alter
                    (Just . innerUpdate . fromMaybe mempty)
                    file
                    acc
                  where
                  innerUpdate =
                    Map.alter
                      (Just . (<> unpackTxResults txResults) . fromMaybe mempty)
                      line
                Nothing -> acc
            Nothing -> acc
        ) mempty vec
      Nothing -> mempty

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
  covMap <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  let
    cs = Map.elems env.dapp.solcByName
    asserts = maybe [] (concatMap assertLocationList . Map.elems . (.asserts)) env.slitherInfo
    covLines = srcMapCov sc covMap cs
  mapM_ (checkAssertionReached covLines) asserts

-- | Helper function for `checkAssertionsCoverage` which checks a single assertion
-- and logs a warning if it wasn't hit
checkAssertionReached :: Map String (Map Int [TxResult]) -> AssertLocation -> IO ()
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
htmlTemplate :: [(FilePath, V.Vector Text)] -> Map FilePath (S.Set Int) -> Map FilePath (Map Int [TxResult]) -> Maybe Text -> Text -> FilePath -> Text
htmlTemplate allFiles runtimeLinesMap covLines projectName timestamp commonPrefix =
  substituteValue coverageTemplate $ buildTemplateContext allFiles runtimeLinesMap covLines projectName timestamp commonPrefix

-- | Build the context object for the mustache template
buildTemplateContext :: [(FilePath, V.Vector Text)] -> Map FilePath (S.Set Int) -> Map FilePath (Map Int [TxResult]) -> Maybe Text -> Text -> FilePath -> Value
buildTemplateContext allFiles runtimeLinesMap covLines projectName timestamp commonPrefix =
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
    , ("files", toMustache filesData)
    ]

-- | Build context for a single file
buildFileContext :: Map FilePath (S.Set Int) -> Map FilePath (Map Int [TxResult]) -> FilePath -> (FilePath, V.Vector Text) -> Value
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
buildLineContext :: S.Set Int -> Map Int [TxResult] -> Int -> Text -> Value
buildLineContext runtimeLines covered lineIndex codeLine =
  let
    lineNum = lineIndex + 1
    results = fromMaybe [] (Map.lookup lineNum covered)
    isActive = lineNum `S.member` runtimeLines
    isCovered = not (null results)
    rowClass
      | not isActive = Nothing
      | isCovered = Just ("row-line-covered" :: Text)
      | otherwise = Just ("row-line-uncovered" :: Text)

  in toMustache $ (Map.fromList :: [(Text, Value)] -> Map Text Value) $ catMaybes
    [ Just ("lineNumber", toMustache $ T.pack $ show lineNum)
    , Just ("sourceCode", toMustache codeLine)
    , fmap (\v -> ("rowClass", toMustache v)) rowClass
    ]

-- | Calculate total statistics across all files
calculateTotalStats :: [(FilePath, V.Vector Text)] -> Map FilePath (S.Set Int) -> Map FilePath (Map Int [TxResult]) -> (Int, Int, Int)
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
