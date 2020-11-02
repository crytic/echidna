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
import Data.Text (Text, append, index, length, unlines)
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

type Filename = Text

markLine :: Int -> Filename -> [(Filename, Text)] -> [(Filename, Text)] 
markLine n cf ls = case splitAt (n-1) ls of
                     (xs, (f,y):ys) | f == cf -> xs ++ [(cf, markLine_ y)] ++ ys
                     _                        -> ls
markLine_   :: Text -> Text
markLine_ l = if Data.Text.length l > 0 && (index l 0 == '|')
              then l 
              else append "|" l

filterLines :: [Maybe (Filename, Int)] -> [(Filename, Text)] -> [(Filename, Text)]
filterLines []                  ls  = ls
filterLines (Nothing      : ns) ls  = filterLines ns ls
filterLines ((Just (f,n)) : ns) ls  = filterLines ns (markLine n f ls)


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

srcMapCov :: SourceCache -> Map BS.ByteString (Set (Int, b)) -> SolcContract -> [Maybe (Text, Int)]
srcMapCov sc s c = nub $ sort $ map (srcMapCodePos sc) $ mapMaybe (srcMapForOpLocation c) $ S.toList $ maybe S.empty (S.map fst) $ M.lookup (stripBytecodeMetadata $ view runtimeCode c) s
                   --catMaybes $ S.toList $ maybe S.empty (S.map (srcMapForOpLocation c . fst)) $ M.lookup (stripBytecodeMetadata $ view runtimeCode c) s

srcMapForOpLocation :: SolcContract -> Int -> Maybe SrcMap
srcMapForOpLocation c n = preview (ix n) (view runtimeSrcmap c <> view creationSrcmap c)

linesByName :: SourceCache -> Map Text (V.Vector BS.ByteString)
linesByName sources =
      ( M.fromList
      . map
          (\(k, v) ->
             (fst (fromJust (M.lookup k (view sourceFiles sources))), v))
      . M.toList
      $ view sourceLines sources
      ) 

