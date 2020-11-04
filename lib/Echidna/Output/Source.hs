{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Echidna.Output.Source where

import Control.Lens hiding (index)
import Data.Maybe (fromJust, mapMaybe)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text, append, index, length, unlines, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (writeFile)
import Data.List (sort, nub)

import EVM.Solidity --(SourceCache, SrcMap, SolcContract, contractName, sourceLines, sourceFiles, runtimeCode, runtimeSrcmap, creationSrcmap)
import EVM.Debug (srcMapCodePos) --, srcMapCode)
import Prelude hiding (unlines, writeFile)

import qualified Data.Vector as V

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S

import Echidna.Exec 
import Echidna.Types.Tx


type FilePathText = Text

saveCoveredCode :: Maybe FilePath -> SourceCache -> [SolcContract] -> CoverageMap -> IO ()
saveCoveredCode (Just d) sc cs s = let fn = d ++ "/covered.txt"
                                       cc = ppCoveredCode sc cs s 
                                   in writeFile fn cc
saveCoveredCode Nothing  _  _  _ = pure ()


-- | Pretty-print the covered code
ppCoveredCode :: SourceCache -> [SolcContract] -> CoverageMap -> Text
ppCoveredCode sc cs s | s == mempty = ""
                      | otherwise   = unlines $ map snd $ concat $ map (\(f,vls) -> 
                                                                           (mempty, (findFile f)) : 
                                                                           (filterLines covLines $ map ((findFile f,) . decodeUtf8) $ V.toList vls)
                                                                       ) allLines  
--map (pack . show) $ S.toList $ allPositions (M.fromList $ map (\c -> (view contractName c,c)) cs) sc  --Lines
                                      where 
                                       allLines = M.toList $ view sourceLines sc
                                       findFile k = fst $ M.findWithDefault ("<no source code>", mempty) k (view sourceFiles sc)
                                       covLines = concat $ map (srcMapCov sc s) cs 

markLine :: Int -> TxResult -> FilePathText -> [(FilePathText, Text)] -> [(FilePathText, Text)] 
markLine n r cf ls = case splitAt (n-1) ls of
                        (xs, (f,y):ys) | f == cf -> xs ++ [(cf, markStringLine r $ unpack y)] ++ ys
                        _                        -> map (\(f,y) -> (f, checkMarkers $ unpack y)) ls

checkMarkers  :: String -> Text
checkMarkers (m1: m2 : m3 : m4: '|':l) = pack $ m1: m2 : m3 : m4   : '|': l
checkMarkers                        l  = pack $ ' ': ' ': ' ': ' ' : '|': l

markStringLine  :: TxResult -> String -> Text
markStringLine r (' ': ' ': ' ': ' ': '|':l) = pack $ getMarker r : ' ' : ' ' : ' ': '|' : l
markStringLine r (m1: ' ': ' ': ' ':  '|':l)  = pack $ case (getMarker r) of
                                                        m | m1 == m -> m1 : ' ' : ' ' : ' ': '|' : l
                                                        m           -> m1 :  m  : ' ' : ' ': '|' : l

markStringLine r (m1: m2 : ' ': ' ': '|':l)  = pack $ case (getMarker r) of
                                                        m | m1 == m -> m1 :  m2  : ' ' : ' ': '|' : l
                                                        m | m2 == m -> m1 :  m2  : ' ' : ' ': '|' : l
                                                        m           -> m1 :  m2  : m   : ' ': '|' : l


markStringLine r (m1: m2 : m3 : ' ': '|':l)  = pack $ case (getMarker r) of
                                                        m | m1 == m -> m1 :  m2  : m3 : ' ': '|' : l
                                                        m | m2 == m -> m1 :  m2  : m3 : ' ': '|' : l
                                                        m | m3 == m -> m1 :  m2  : m3 : ' ': '|' : l
                                                        m           -> m1 :  m2  : m3 : m  : '|' : l

markStringLine _ (_: _ : _ : _ : '|':_)  = error "impossible to add another marker"
markStringLine r l = pack $ getMarker r : ' ' : ' ' : ' ' : '|' : l

getMarker :: TxResult -> Char
getMarker Success       = '*'
getMarker ErrorRevert   = 'r' 
getMarker ErrorOutOfGas = 'o'
getMarker _             = 'e'

filterLines :: [Maybe (FilePathText, Int, TxResult)] -> [(FilePathText, Text)] -> [(FilePathText, Text)]
filterLines []                    ls  = ls
filterLines (Nothing        : ns) ls  = filterLines ns ls
filterLines ((Just (f,n,r)) : ns) ls  = filterLines ns (markLine n r f ls)


-- map (pack . show) $ S.toList $ allPositions (M.fromList $ map (\c -> (view contractName c,c)) cs) sc
{-
allPositions :: Map k SolcContract -> SourceCache -> Set (Text, Int)
allPositions solcByName sources =
      ( S.fromList
      . mapMaybe (srcMapCodePos sources)
      . toList
      $ mconcat
        ( solcByName
        & M.elems
        & map (\x -> view runtimeSrcmap x <> view creationSrcmap x)
        )
      )
-}

srcMapCov :: SourceCache -> Map BS.ByteString (Set (Int, TxResult)) -> SolcContract -> [Maybe (FilePathText, Int, TxResult)]
srcMapCov sc s c = nub $ sort $ map (srcMapCodePos' sc) $ mapMaybe (srcMapForOpLocation c) $ S.toList $ maybe S.empty (id) $ M.lookup (stripBytecodeMetadata $ view runtimeCode c) s
                   --catMaybes $ S.toList $ maybe S.empty (S.map (srcMapForOpLocation c . fst)) $ M.lookup (stripBytecodeMetadata $ view runtimeCode c) s


srcMapCodePos' sc (n, r) = case srcMapCodePos sc n of 
                             Just (t,n') -> Just (t,n',r)
                             _          -> Nothing

srcMapForOpLocation :: SolcContract -> (Int,TxResult) -> Maybe (SrcMap, TxResult)
srcMapForOpLocation c (n,r) = case preview (ix n) (view runtimeSrcmap c <> view creationSrcmap c) of
                                Just sm -> Just (sm,r)
                                _       -> Nothing

linesByName :: SourceCache -> Map Text (V.Vector BS.ByteString)
linesByName sources =
      ( M.fromList
      . map
          (\(k, v) ->
             (fst (fromJust (M.lookup k (view sourceFiles sources))), v))
      . M.toList
      $ view sourceLines sources
      ) 

