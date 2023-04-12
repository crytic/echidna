module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Foldable
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (nub, sort)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack, toLower)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Vector qualified as V
import HTMLEntities.Text qualified as HTML
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import EVM.Debug (srcMapCodePos)
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Coverage (CoverageMap, CoverageInfo)
import Echidna.Types.Tx (TxResult(..))
import Echidna.Types.Signature (getBytecodeMetadata)

type FilePathText = Text

data CoverageFileType = Lcov | Html | Txt deriving (Eq, Show)

instance ToJSON CoverageFileType where
  toJSON = toJSON . show

instance FromJSON CoverageFileType where
  parseJSON = withText "CoverageFileType" $ readFn . toLower where
    readFn "lcov" = pure Lcov
    readFn "html" = pure Html
    readFn "text" = pure Txt
    readFn "txt"  = pure Txt
    readFn _ = fail "could not parse CoverageFileType"

coverageFileExtension :: CoverageFileType -> String
coverageFileExtension Lcov = ".lcov"
coverageFileExtension Html = ".html"
coverageFileExtension Txt = ".txt"

saveCoverages :: [CoverageFileType] -> Int -> FilePath -> SourceCache -> [SolcContract] -> CoverageMap -> IO ()
saveCoverages fileTypes seed d sc cs s = mapM_ (\ty -> saveCoverage ty seed d sc cs s) fileTypes

saveCoverage :: CoverageFileType -> Int -> FilePath -> SourceCache -> [SolcContract] -> CoverageMap -> IO ()
saveCoverage fileType seed d sc cs s = let extension = coverageFileExtension fileType
                                           fn = d </> "covered." <> show seed <> extension
                                           cc = ppCoveredCode fileType sc cs s
                                       in do
                                         createDirectoryIfMissing True d
                                         writeFile fn cc

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> CoverageMap -> Text
ppCoveredCode fileType sc cs s | s == mempty = "Coverage map is empty"
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
            marked = markLines fileType srcLines runtimeLines (fromMaybe M.empty (M.lookup srcPath covLines))
        in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
      -- ^ Pretty print individual file coverage
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
      changeFileName fn = case fileType of
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
markLines :: CoverageFileType -> V.Vector Text -> S.Set Int -> M.Map Int [TxResult] -> V.Vector Text
markLines fileType codeLines runtimeLines resultMap =
  V.map markLine . V.filter shouldUseLine $ V.indexed codeLines
  where
  shouldUseLine (i, _) = case fileType of
    Lcov -> i + 1 `elem` runtimeLines
    _ -> True
  markLine (i, codeLine) =
    let n = i + 1
        results  = fromMaybe [] (M.lookup n resultMap)
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
          Lcov -> pack $ printf "DA:%d,%d" n (length results)
          _ -> pack $ printf " %*d | %-4s| %s" lineNrSpan n markers (wrapLine codeLine)

    in result
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
