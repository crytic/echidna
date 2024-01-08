{-# LANGUAGE ViewPatterns #-}

module Echidna.Output.Source where

import Prelude hiding (writeFile)

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text, pack, toLower)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Vector qualified as V
import Data.Vector.Unboxed.Mutable qualified as VU
import HTMLEntities.Text qualified as HTML
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import EVM.Dapp (srcMapCodePos)
import EVM.Solidity (SourceCache(..), SrcMap, SolcContract(..))

import Echidna.Types.Coverage (OpIx, unpackTxResults, CoverageMap)
import Echidna.Types.Tx (TxResult(..))

saveCoverages
  :: [CoverageFileType]
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> CoverageMap
  -> IO ()
saveCoverages fileTypes seed d sc cs s =
  mapM_ (\ty -> saveCoverage ty seed d sc cs s) fileTypes

saveCoverage
  :: CoverageFileType
  -> Int
  -> FilePath
  -> SourceCache
  -> [SolcContract]
  -> CoverageMap
  -> IO ()
saveCoverage fileType seed d sc cs covMap = do
  let extension = coverageFileExtension fileType
      fn = d </> "covered." <> show seed <> extension
  cc <- ppCoveredCode fileType sc cs covMap
  createDirectoryIfMissing True d
  writeFile fn cc

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

-- | Pretty-print the covered code
ppCoveredCode :: CoverageFileType -> SourceCache -> [SolcContract] -> CoverageMap -> IO Text
ppCoveredCode fileType sc cs s | null s = pure "Coverage map is empty"
  | otherwise = do
  -- List of covered lines during the fuzzing campaing
  covLines <- srcMapCov sc s cs
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
      Html -> "<code>" : ls ++ ["", "</code>","<br />"]
      Txt  -> ls
    -- ^ Alter file contents, in the case of html encasing it in <code> and adding a line break
  pure $ topHeader <> T.unlines (map ppFile allFiles)

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
srcMapCov :: SourceCache -> CoverageMap -> [SolcContract] -> IO (Map FilePath (Map Int [TxResult]))
srcMapCov sc covMap contracts = do
  Map.unionsWith Map.union <$> mapM linesCovered contracts
  where
  linesCovered :: SolcContract -> IO (Map FilePath (Map Int [TxResult]))
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
