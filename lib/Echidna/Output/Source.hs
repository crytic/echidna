{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Output.Source where

import Control.Lens
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, unlines, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.List (nub)

import EVM.Solidity (SourceCache, SrcMap, SolcContract, sourceLines, sourceFiles, runtimeCode, runtimeSrcmap, creationSrcmap)
import EVM.Debug (srcMapCodePos)
import Prelude hiding (unlines, writeFile)

import qualified Data.Vector as V

import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Exec 
import Echidna.Types.Tx
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
  let allLines = M.toList $ sc ^. sourceLines
  -- ^ Collect all the possible lines from all the files
      findFile k = fst $ M.findWithDefault ("<no source code>", mempty) k (sc ^. sourceFiles)
  -- ^ Auxiliary function to get the path for each source file
      covLines = concatMap (srcMapCov sc s) cs
  -- ^ List of covered lines during the fuzzing campaing
  in unlines $ map snd $ concatMap (\(f,vls) ->
    (mempty, findFile f) :                                                   -- Add a header for each source file to show its complete path
      filterLines covLines (map ((findFile f,) . decodeUtf8) $ V.toList vls) -- Show the source code for each file with its covered line.
                                   ) allLines

-- | Filter the lines per file, marking each line
filterLines :: [Maybe (FilePathText, Int, TxResult)] -> [(FilePathText, Text)] -> [(FilePathText, Text)]
filterLines []                  ls  = ls
filterLines (Nothing      : ns) ls  = filterLines ns ls
filterLines (Just (f,n,r) : ns) ls  = filterLines ns (markLine n r f ls)

-- | Mark one particular line, from a list of lines, keeping the order of them 
markLine :: Int -> TxResult -> FilePathText -> [(FilePathText, Text)] -> [(FilePathText, Text)] 
markLine n r cf ls = case splitAt (n-1) ls of
  (xs, (f,y):ys) | f == cf -> xs ++ [(cf, pack $ markStringLine r $ unpack y)] ++ ys
  _                        -> map (\(f,y) -> (f, pack $ checkMarkers $ unpack y)) ls

-- | Header preppend to each line
markerHeader :: String
markerHeader = "    |"

-- | Add space for markers if necessary
checkMarkers  :: String -> String
checkMarkers l@(_:_:_:_:'|':_) = l
checkMarkers l                 = markerHeader ++ l

-- | Add a proper marker to a line, and convert it to Text
markStringLine  :: TxResult -> String -> String
markStringLine r (' ': ' ': ' ': ' ': '|': l) = getMarker r : ' ' : ' ' : ' ': '|' : l
markStringLine r (m1 : ' ': ' ': ' ': '|': l) = case getMarker r of
  m | m1 == m -> m1 : ' ' : ' ' : ' ': '|' : l
  m           -> m1 :  m  : ' ' : ' ': '|' : l

markStringLine r (m1 : m2 : ' ': ' ': '|': l) = case getMarker r of
  m | m1 == m -> m1 :  m2  : ' ' : ' ': '|' : l
  m | m2 == m -> m1 :  m2  : ' ' : ' ': '|' : l
  m           -> m1 :  m2  : m   : ' ': '|' : l


markStringLine r (m1 : m2 : m3 : ' ': '|': l) = case getMarker r of
  m | m1 == m -> m1 :  m2  : m3 : ' ': '|' : l
  m | m2 == m -> m1 :  m2  : m3 : ' ': '|' : l
  m | m3 == m -> m1 :  m2  : m3 : ' ': '|' : l
  m           -> m1 :  m2  : m3 : m  : '|' : l

markStringLine _ (_: _ : _ : _ : '|':_) = error "impossible to add another marker"
markStringLine r l = getMarker r : ' ' : ' ' : ' ': '|' : l

-- | Select the proper marker, according to the result of the transaction
getMarker :: TxResult -> Char
getMarker Success       = '*'
getMarker ErrorRevert   = 'r' 
getMarker ErrorOutOfGas = 'o'
getMarker _             = 'e'

-- | Given a source cache, a coverage map, a contract returns a list of covered lines
srcMapCov :: SourceCache -> CoverageMap -> SolcContract -> [Maybe (FilePathText, Int, TxResult)]
srcMapCov sc s c = nub $                                               -- Deduplicate results 
                   map (srcMapCodePosResult sc) $                      -- Get the filename, number of line and tx result
                   mapMaybe (srcMapForOpLocation c) $                  -- Get the mapped line and tx result
                   S.toList $ fromMaybe S.empty $                      -- Convert from Set to list
                   M.lookup (getBytecodeMetadata $ c ^. runtimeCode) s -- Get the coverage information of the current contract

-- | Given a source cache, a mapped line, return a tuple with the filename, number of line and tx result
srcMapCodePosResult :: SourceCache -> (SrcMap, TxResult) -> Maybe (Text, Int, TxResult)
srcMapCodePosResult sc (n, r) = case srcMapCodePos sc n of 
  Just (t,n') -> Just (t,n',r)
  _           -> Nothing

-- | Given a contract, and tuple as coverage, return the corresponding mapped line (if any)
srcMapForOpLocation :: SolcContract -> (PC, OpIx, FrameCount, TxResult) -> Maybe (SrcMap, TxResult)
srcMapForOpLocation c (_,n,_,r) = case preview (ix n) (c ^. runtimeSrcmap <> c ^. creationSrcmap) of
  Just sm -> Just (sm,r)
  _       -> Nothing
