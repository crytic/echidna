{-# LANGUAGE ViewPatterns #-}

module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List (nub, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import HTMLEntities.Text qualified as HTML
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import EVM.Dapp (srcMapCodePos, DappInfo(..))
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Coverage (OpIx, unpackTxResults, FrozenCoverageMap, CoverageFileType (..), mergeCoverageMaps)
import Echidna.Types.Tx (TxResult(..))
import Echidna.SourceAnalysis.Slither (AssertLocation(..), assertLocationList, SlitherInfo(..))

saveCoverages
  :: Env
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> IO ()
saveCoverages env seed d sc cs = do
  let fileTypes = env.cfg.campaignConf.coverageFormats
  coverage <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  mapM_ (\ty -> saveCoverage ty seed d sc cs coverage) fileTypes

saveCoverage
  :: CoverageFileType
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> FrozenCoverageMap
  -> IO ()
saveCoverage fileType seed d sc cs covMap = do
  let extension = coverageFileExtension fileType
      fn = d </> "covered." <> show seed <> extension
      cc = ppCoveredCode fileType sc cs covMap
  createDirectoryIfMissing True d
  writeFile fn cc

coverageFileExtension :: CoverageFileType -> String
coverageFileExtension Lcov = ".lcov"
coverageFileExtension Html = ".html"
coverageFileExtension Txt = ".txt"

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> FrozenCoverageMap -> Text
ppCoveredCode fileType sc cs s | null s = "Coverage map is empty"
  | otherwise =
  let
    -- List of covered lines during the fuzzing campaign
    covLines = srcMapCov sc s cs
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
              ".e { background-color: #afa; }" <> -- executed
              ".r { background-color: #ffa; }" <> -- reverted
              ".u { background-color: #faa; }" <> -- unexecuted
              ".n { background-color: #eee; }" <> -- neutral
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
      Html -> "<code>" : ls ++ ["", "</code>","<br />"]
      Txt  -> ls
    -- ^ Alter file contents, in the case of html encasing it in <code> and adding a line break
  in topHeader <> T.unlines (map ppFile allFiles)

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
