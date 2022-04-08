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
import Data.List (nub, sort)
import Text.Printf (printf)

import EVM.Solidity (SourceCache, SrcMap, SolcContract, sourceLines, sourceFiles, runtimeCode, runtimeSrcmap, creationSrcmap)
import EVM.Debug (srcMapCodePos)
import Prelude hiding (writeFile)

import qualified Data.Vector as V

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Echidna.Types.Coverage (CoverageMap, CoverageInfo, TxEnded)
import Echidna.Types.Tx (TxResult(..))
import Echidna.Types.Signature (getBytecodeMetadata)

type FilePathText = Text

saveCoverage :: Maybe FilePath -> SourceCache -> [SolcContract] -> CoverageMap -> IO ()
saveCoverage (Just d) sc cs s = let fn t = d ++ "/covered." ++ show t ++ ".txt"
                                    cc = ppCoveredCode sc cs s
                                in getSystemTime >>= \t -> writeFile (fn $ systemSeconds t) cc
saveCoverage Nothing  _  _  _ = pure ()


-- | Pretty-print the covered code
ppCoveredCode :: SourceCache -> [SolcContract] -> CoverageMap -> Text
ppCoveredCode sc cs s | s == mempty = "Coverage map is empty"
                      | otherwise   =
  let allFiles = zipWith (\(srcPath, _rawSource) srcLines -> (srcPath, V.map decodeUtf8 srcLines))
                   (sc ^. sourceFiles)
                   (sc ^. sourceLines)
  -- ^ Collect all the possible lines from all the files
      covLines = srcMapCov sc s cs
  -- ^ List of covered lines during the fuzzing campaing
      ppFile (srcPath, srcLines) =
        let marked = markLines srcLines (fromMaybe M.empty (M.lookup srcPath covLines))
        in T.unlines (srcPath : V.toList marked)
  in T.intercalate "\n" $ map ppFile allFiles

-- | Mark one particular line, from a list of lines, keeping the order of them
markLines :: V.Vector Text -> M.Map Int [(TxResult, TxEnded)] -> V.Vector Text
markLines codeLines resultMap = V.map markLine (V.indexed codeLines)
  where
    markLine (i, codeLine) =
      let results = fromMaybe [] (M.lookup (i+1) resultMap)
      in pack $ printf "%-4s| %s" (sort $ simplifyMarkers $ nub $ getMarker <$> results) (unpack codeLine)

-- | Select the proper marker, according to the result of the transaction
getMarker :: (TxResult, TxEnded) -> Char
getMarker (ReturnTrue,  _)   = '*'
getMarker (ReturnFalse, _)   = '*'
getMarker (Stop, _)          = '*'
getMarker (ErrorRevert, b)   = if b then 'R' else 'r'
getMarker (ErrorOutOfGas, _) = 'o'
getMarker _                  = 'e'

simplifyMarkers :: String -> String
simplifyMarkers cs = if 'R' `elem` cs && 'r' `elem` cs
  then filter (/= 'r') cs
  else cs

-- | Given a source cache, a coverage map, a contract returns a list of covered lines
srcMapCov :: SourceCache -> CoverageMap -> [SolcContract] -> M.Map FilePathText (M.Map Int [(TxResult, TxEnded)])
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
srcMapCodePosResult :: SourceCache -> (SrcMap, (TxResult, TxEnded)) -> Maybe (Text, Int, (TxResult, TxEnded))
srcMapCodePosResult sc (n, r) = case srcMapCodePos sc n of
  Just (t,n') -> Just (t,n',r)
  _           -> Nothing

-- | Given a contract, and tuple as coverage, return the corresponding mapped line (if any)
srcMapForOpLocation :: SolcContract -> CoverageInfo -> Maybe (SrcMap, (TxResult, TxEnded))
srcMapForOpLocation c (_,n,_,r,b,_) = case preview (ix n) (c ^. runtimeSrcmap <> c ^. creationSrcmap) of
  Just sm -> Just (sm,(r,b))
  _       -> Nothing
