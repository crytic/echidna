module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Data.Foldable
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (nub, sort)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Vector qualified as V
import HTMLEntities.Text qualified as HTML
import Text.Printf (printf)

import EVM.Debug (srcMapCodePos)
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Coverage (CoverageMap, CoverageInfo)
import Echidna.Types.Tx (TxResult(..))
import Echidna.Types.Signature (getBytecodeMetadata)
import System.FilePath ((</>))

type FilePathText = Text

saveCoverage :: Bool -> Int -> FilePath -> SourceCache -> [SolcContract] -> CoverageMap -> IO ()
saveCoverage isHtml seed d sc cs s = let extension = if isHtml then ".html" else ".txt"
                                         fn = d </> "covered." <> show seed <> extension
                                         cc = ppCoveredCode isHtml sc cs s
                                       in writeFile fn cc

-- | Pretty-print the covered code
ppCoveredCode :: Bool -> SourceCache -> [SolcContract] -> CoverageMap -> Text
ppCoveredCode isHtml sc cs s | s == mempty = "Coverage map is empty"
                             | otherwise   =
  let allFiles = zipWith (\(srcPath, _rawSource) srcLines -> (srcPath, V.map decodeUtf8 srcLines))
                   sc.files
                   sc.lines
      -- ^ Collect all the possible lines from all the files
      covLines = srcMapCov sc s cs
      -- ^ List of covered lines during the fuzzing campaing
      runtimeLinesMap = buildRuntimeLinesMap sc cs
      -- ^ Excludes lines such as comments or blanks
      ppFile (srcPath, srcLines) =
        let runtimeLines = fromMaybe mempty $ M.lookup srcPath runtimeLinesMap
            marked = markLines isHtml srcLines runtimeLines (fromMaybe M.empty (M.lookup srcPath covLines))
        in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
      -- ^ Pretty print individual file coverage
      topHeader
        | isHtml = "<style> code { white-space: pre-wrap; display: block; background-color: #eee; }" <>
                   ".executed { background-color: #afa; }" <>
                   ".reverted { background-color: #ffa; }" <>
                   ".unexecuted { background-color: #faa; }" <>
                   ".neutral { background-color: #eee; }" <>
                   "</style>"
        | otherwise = ""
      -- ^ Text to add to top of the file
      changeFileName fn
        | isHtml = "<b>" <> HTML.text fn <> "</b>"
        | otherwise = fn
      -- ^ Alter file name, in the case of html turning it into bold text
      changeFileLines ls
        | isHtml = "<code>" : ls ++ ["", "</code>","<br />"]
        | otherwise = ls
      -- ^ Alter file contents, in the case of html encasing it in <code> and adding a line break
  in topHeader <> T.unlines (map ppFile allFiles)

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: Bool -> V.Vector Text -> S.Set Int -> M.Map Int [TxResult] -> V.Vector Text
markLines isHtml codeLines runtimeLines resultMap =
  V.map markLine (V.indexed codeLines)
  where
  markLine (i, codeLine) =
    let n = i + 1
        results  = fromMaybe [] (M.lookup n resultMap)
        markers = sort $ nub $ getMarker <$> results
        wrapLine :: Text -> Text
        wrapLine line
          | isHtml = "<span class='" <> cssClass <> "'>" <>
                        HTML.text line <>
                     "</span>"
          | otherwise = line
          where
          cssClass = if n `elem` runtimeLines then getCSSClass markers else "neutral"

    in pack $ printf " %*d | %-4s| %s" lineNrSpan n markers (wrapLine codeLine)
  lineNrSpan = length . show $ V.length codeLines + 1

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
srcMapCov :: SourceCache -> CoverageMap -> [SolcContract] -> M.Map FilePathText (M.Map Int [TxResult])
srcMapCov sc s contracts =
  M.map (M.fromListWith (++)) .
  M.fromListWith (++) .
  map (\(srcPath, line, txResult) -> (srcPath, [(line, [txResult])])) .
  nub .                                              -- Deduplicate results
  mapMaybe (srcMapCodePosResult sc) $                -- Get the filename, number of line and tx result
  concatMap mapContract contracts
  where
    mapContract c =
      mapMaybe (srcMapForOpLocation c) .             -- Get the mapped line and tx result
      S.toList . fromMaybe S.empty $                 -- Convert from Set to list
      M.lookup (getBytecodeMetadata c.runtimeCode) s -- Get the coverage information of the current contract

-- | Given a source cache, a mapped line, return a tuple with the filename, number of line and tx result
srcMapCodePosResult :: SourceCache -> (SrcMap, TxResult) -> Maybe (Text, Int, TxResult)
srcMapCodePosResult sc (n, r) = case srcMapCodePos sc n of
  Just (t,n') -> Just (t,n',r)
  _           -> Nothing

-- | Given a contract, and tuple as coverage, return the corresponding mapped line (if any)
srcMapForOpLocation :: SolcContract -> CoverageInfo -> Maybe (SrcMap, TxResult)
srcMapForOpLocation contract (_,n,_,r) =
  case Seq.lookup n (contract.runtimeSrcmap <> contract.creationSrcmap) of
    Just sm -> Just (sm,r)
    _       -> Nothing

-- | Builds a Map from file paths to lines that can be executed, this excludes
-- for example lines with comments
buildRuntimeLinesMap :: SourceCache -> [SolcContract] -> M.Map Text (S.Set Int)
buildRuntimeLinesMap sc contracts =
  M.fromListWith (<>)
    [(k, S.singleton v) | (k, v) <- mapMaybe (srcMapCodePos sc) srcMaps]
  where
  srcMaps = concatMap
    (\c -> toList $ c.runtimeSrcmap <> c.creationSrcmap) contracts
