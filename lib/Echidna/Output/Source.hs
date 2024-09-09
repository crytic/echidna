{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.IORef (readIORef, IORef)
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.TLS.GHC (allTLS, TLS)
import Data.Vector qualified as V
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Mutable qualified as VU
import HTMLEntities.Text qualified as HTML
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import EVM.Dapp (srcMapCodePos, DappInfo(..))
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Coverage (OpIx, unpackTxResults, CoverageMap, CoverageFileType (..), StatsMap, StatsMapV, StatsInfo)
import Echidna.Types.Tx (TxResult(..))
import Echidna.SourceAnalysis.Slither (AssertLocation(..), assertLocationList, SlitherInfo(..))
import EVM.Types (W256)

zipSumStats :: IO [StatsInfo] -> IO [StatsInfo] -> IO [StatsInfo]
zipSumStats v1 v2 = do
  vec1 <- v1
  vec2 <- v2
  return [exec1 + exec2 | exec1 <- vec1 | exec2 <- vec2]

combineStats :: TLS (IORef StatsMap) -> IO StatsMapV
combineStats statsRef = do
  threadStats' <- allTLS statsRef
  threadStats <-  mapM readIORef threadStats' :: IO [StatsMap]
  let statsLists = map (Map.map mvToList) threadStats :: [Map EVM.Types.W256 (IO [StatsInfo])]
  traverse (U.fromList <$>) $ Map.unionsWith zipSumStats statsLists
  where
    mvToList :: (VU.Unbox a) => VU.IOVector a -> IO [a]
    mvToList = fmap U.toList . U.freeze

saveCoverages
  :: Env
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> IO ()
saveCoverages env seed d sc cs = do
  let fileTypes = env.cfg.campaignConf.coverageFormats
  coverage <- readIORef env.coverageRef
  stats <- combineStats env.statsRef
  mapM_ (\ty -> saveCoverage ty seed d sc cs coverage stats) fileTypes

saveCoverage
  :: CoverageFileType
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> CoverageMap
  -> StatsMapV
  -> IO ()
saveCoverage fileType seed d sc cs covMap statMap = do
  let extension = coverageFileExtension fileType
      fn = d </> "covered." <> show seed <> extension
  cc <- ppCoveredCode fileType sc cs covMap statMap
  createDirectoryIfMissing True d
  writeFile fn cc

coverageFileExtension :: CoverageFileType -> String
coverageFileExtension Lcov = ".lcov"
coverageFileExtension Html = ".html"
coverageFileExtension Txt = ".txt"

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> CoverageMap -> StatsMapV -> IO Text
ppCoveredCode fileType sc cs s sm | null s = pure "Coverage map is empty"
  | otherwise = do
  -- List of covered lines during the fuzzing campaign
  covLines <- srcMapCov sc s sm cs
  let
    -- Collect all the possible lines from all the files
    allFiles = (\(path, src) -> (path, V.fromList (decodeUtf8 <$> BS.split 0xa src))) <$> Map.elems sc.files
    -- Excludes lines such as comments or blanks
    runtimeLinesMap = buildRuntimeLinesMap sc cs
    -- Pretty print individual file coverage
    ppFile (srcPath, srcLines) =
      let runtimeLines = fromMaybe mempty $ Map.lookup srcPath runtimeLinesMap
          marked = markLines fileType srcLines runtimeLines (fromMaybe Map.empty (Map.lookup srcPath covLines))
      in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
    topHeader = case fileType of
      Lcov -> "TN:\n"
      Html -> "<style> code { white-space: pre-wrap; display: block; background-color: #eee; }" <>
              ".executed { background-color: #afa; }" <>
              ".reverted { background-color: #ffa; }" <>
              ".unexecuted { background-color: #faa; }" <>
              ".neutral { background-color: #eee; }" <>
              "</style>"
      Txt  -> ""
    -- ^ Text to add to top of the file
    changeFileName (T.pack -> fn) = case fileType of
      Lcov -> "SF:" <> fn
      Html -> "<b>" <> HTML.text fn <> "</b>"
      Txt  -> fn
    -- ^ Alter file name, in the case of html turning it into bold text
    changeFileLines ls = case fileType of
      Lcov -> ls ++ ["end_of_record"]
      Html -> "<br /><b>Legend:</b> Line # | Execs # | Reverts # | Code<br /><code>" : ls ++ ["", "</code>","<br />"]
      Txt  -> ls
    -- ^ Alter file contents, in the case of html encasing it in <code> and adding a line break
  pure $ topHeader <> T.unlines (map ppFile allFiles)

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: CoverageFileType -> V.Vector Text -> S.Set Int -> Map Int ([TxResult], StatsInfo) -> V.Vector Text
markLines fileType codeLines runtimeLines resultMap =
  V.map markLine . V.filter shouldUseLine $ V.indexed codeLines
  where
  shouldUseLine (i, _) = case fileType of
    Lcov -> i + 1 `elem` runtimeLines
    _ -> True
  markLine (i, codeLine) =
    let n = i + 1
        (results, execs) = fromMaybe ([], 0) (Map.lookup n resultMap)
        reverts = 0
        markers = sort $ nub $ getMarker <$> results
        wrapLine :: Text -> Text
        wrapLine line = case fileType of
          Html -> "<span class='" <> cssClass <> "'>" <>
                        HTML.text line <>
                     "</span>"
          _ -> line
          where
          cssClass = if n `elem` runtimeLines then getCSSClass markers else "neutral"
        result = case fileType of
          Lcov -> pack $ printf "DA:%d,%d" n execs
          Html -> pack $ printf "%*d | %4s | %4s | %-4s| %s" lineNrSpan n (prettyCount execs) (prettyCount reverts) markers (wrapLine codeLine)
          _    -> pack $ printf "%*d | %-4s| %s" lineNrSpan n markers (wrapLine codeLine)

    in result
  lineNrSpan = length . show $ V.length codeLines + 1
  prettyCount x = prettyCount' x 0
  prettyCount' x n | x >= 1000          = prettyCount' (x `div` 1000) (n + 1)
                   | x < 1000 && n == 0 = show x
                   | otherwise          = show x <> [" kMGTPEZY" !! n]

getCSSClass :: String -> Text
getCSSClass markers =
  case markers of
   []                      -> "unexecuted"
   _  | '*' `elem` markers -> "executed"
   _                       -> "reverted"

-- | Select the proper marker, according to the result of the transaction
getMarker :: TxResult -> Char
getMarker ReturnTrue    = '*'
getMarker ReturnFalse   = '*'
getMarker Stop          = '*'
getMarker ErrorRevert   = 'r'
getMarker ErrorOutOfGas = 'o'
getMarker _             = 'e'

-- | Given a source cache, a coverage map, a contract returns a list of covered lines
srcMapCov :: SourceCache -> CoverageMap -> StatsMapV -> [SolcContract] -> IO (Map FilePath (Map Int ([TxResult], StatsInfo)))
srcMapCov sc covMap statMap contracts = do
  Map.unionsWith Map.union <$> mapM linesCovered contracts
  where
  linesCovered :: SolcContract -> IO (Map FilePath (Map Int ([TxResult], StatsInfo)))
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
                      updateLine
                      line
                  updateLine (Just (r, s)) = Just ((<> unpackTxResults txResults) r, maxStats s idxStats)
                  updateLine Nothing = Just (unpackTxResults txResults, idxStats)
                  fileStats = Map.lookup c.runtimeCodehash statMap
                  idxStats = maybe 0 (U.! opIx) fileStats
                  maxStats = max
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
  -> StatsMapV
  -> IO ()
checkAssertionsCoverage sc env sm = do
  let
    cs = Map.elems env.dapp.solcByName
    asserts = maybe [] (concatMap assertLocationList . Map.elems . (.asserts)) env.slitherInfo
  covMap <- readIORef env.coverageRef
  covLines <- srcMapCov sc covMap sm cs
  mapM_ (checkAssertionReached covLines) asserts

-- | Helper function for `checkAssertionsCoverage` which checks a single assertion
-- and logs a warning if it wasn't hit
checkAssertionReached :: Map String (Map Int ([TxResult], StatsInfo)) -> AssertLocation -> IO ()
checkAssertionReached covLines assert =
  maybe
    warnAssertNotReached checkCoverage
    (Map.lookup assert.filenameAbsolute $ fmap (fmap fst) covLines)
  where
   checkCoverage coverage = let lineNumbers = Map.keys coverage in
     unless ((head assert.assertLines) `elem` lineNumbers) warnAssertNotReached
   warnAssertNotReached =
    putStrLn $ "WARNING: assertion at file: " ++ assert.filenameRelative
       ++ " starting at line: " ++ show (head assert.assertLines) ++ " was never reached"
