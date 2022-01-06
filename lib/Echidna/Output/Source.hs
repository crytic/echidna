{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.Output.Source where

import Control.Lens
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.List (nub)
import Text.Printf (printf)
import qualified HTMLEntities.Text as HTML

import EVM.Solidity (SourceCache, SrcMap, SolcContract, sourceLines, sourceFiles, runtimeCode, runtimeSrcmap, creationSrcmap)
import EVM.Debug (srcMapCodePos)
import Prelude hiding (writeFile)

import qualified Data.Vector as V

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Echidna.Types.Coverage (CoverageMap, CoverageInfo)
import Echidna.Types.Tx (TxResult(..))
import Echidna.Types.Signature (getBytecodeMetadata)

type FilePathText = Text

saveCoverage :: Bool -> Maybe FilePath -> SourceCache -> [SolcContract] -> CoverageMap -> IO ()
saveCoverage isHtml (Just d) sc cs s = let filepath = if isHtml then ".html" else ".txt"
                                           fn t = d ++ "/covered." ++ show t ++ filepath
                                           cc = ppCoveredCode isHtml sc cs s
                                       in getSystemTime >>= \t -> writeFile (fn $ systemSeconds t) cc
saveCoverage _ Nothing  _  _  _ = pure ()


-- | Pretty-print the covered code
ppCoveredCode :: Bool -> SourceCache -> [SolcContract] -> CoverageMap -> Text
ppCoveredCode isHtml sc cs s | s == mempty = "Coverage map is empty"
                             | otherwise   =
  let allFiles = zipWith (\(srcPath, _rawSource) srcLines -> (srcPath, V.map decodeUtf8 srcLines))
                   (sc ^. sourceFiles)
                   (sc ^. sourceLines)
  -- ^ Collect all the possible lines from all the files
      covLines = srcMapCov sc s cs
  -- ^ List of covered lines during the fuzzing campaing
      ppFile (srcPath, srcLines) =
        let marked = markLines isHtml srcLines (fromMaybe M.empty (M.lookup srcPath covLines))
        in T.unlines (changeFileName srcPath : changeFileLines (V.toList marked))
  -- ^ Pretty print individual file coverage
      topHeader
        | isHtml = "<style> code { white-space: pre-wrap; display: block; background-color: #aaa; } </style>"
        | otherwise = ""
  -- ^ Text to add to top of the file
      changeFileName fn
        | isHtml = "<b>" `T.append` HTML.text fn `T.append` "</b>"
        | otherwise = fn
  -- ^ Alter file name, in the case of html turning it into bold text
      changeFileLines ls
        | isHtml = "<code>" : ls ++ ["", "</code>","<br />"]
        | otherwise = ls
  -- ^ Alter file contents, in the case of html encasing it in <code> and adding a line break
  in topHeader `T.append` T.unlines (map ppFile allFiles)

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: Bool -> V.Vector Text -> M.Map Int [TxResult] -> V.Vector Text
markLines isHtml codeLines resultMap = V.map markLine (V.indexed codeLines)
  where
    codeLineLen = length . show . (+ 1) $ V.length codeLines
    markLine (i, codeLine) =
      let ii = i+1
          results  = fromMaybe [] (M.lookup ii resultMap)
          linePost :: Text
          linePost = if not isHtml then "" else "</span>"
          linePre  :: Text
          linePre  = if not isHtml then "" else "<span style='background-color: #" `T.append` (if null results then "faa" else "afa") `T.append` ";'>"
      in pack $ printf " %d%s | %-4s| %s%s%s" ii (replicate (codeLineLen - length (show ii)) ' ') (getMarker <$> results) linePre (unpack $ (if isHtml then HTML.text else id) codeLine) linePost

-- | Select the proper marker, according to the result of the transaction
getMarker :: TxResult -> Char
getMarker Success       = '*'
getMarker ErrorRevert   = 'r'
getMarker ErrorOutOfGas = 'o'
getMarker _             = 'e'

-- | Given a source cache, a coverage map, a contract returns a list of covered lines
srcMapCov :: SourceCache -> CoverageMap -> [SolcContract] -> M.Map FilePathText (M.Map Int [TxResult])
srcMapCov sc s contracts =
  M.map (M.fromListWith (++)) .
  M.fromListWith (++) .
  map (\(srcPath, line, txResult) -> (srcPath, [(line, [txResult])])) .
  nub .                                               -- Deduplicate results
  mapMaybe (srcMapCodePosResult sc) $                 -- Get the filename, number of line and tx result
  concatMap mapContract contracts
  where
    mapContract c =
      mapMaybe (srcMapForOpLocation c) .                  -- Get the mapped line and tx result
      S.toList . fromMaybe S.empty $                      -- Convert from Set to list
      M.lookup (getBytecodeMetadata $ c ^. runtimeCode) s -- Get the coverage information of the current contract

-- | Given a source cache, a mapped line, return a tuple with the filename, number of line and tx result
srcMapCodePosResult :: SourceCache -> (SrcMap, TxResult) -> Maybe (Text, Int, TxResult)
srcMapCodePosResult sc (n, r) = case srcMapCodePos sc n of
  Just (t,n') -> Just (t,n',r)
  _           -> Nothing

-- | Given a contract, and tuple as coverage, return the corresponding mapped line (if any)
srcMapForOpLocation :: SolcContract -> CoverageInfo -> Maybe (SrcMap, TxResult)
srcMapForOpLocation c (_,n,_,r) = case preview (ix n) (c ^. runtimeSrcmap <> c ^. creationSrcmap) of
  Just sm -> Just (sm,r)
  _       -> Nothing
