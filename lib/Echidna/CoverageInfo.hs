module Echidna.CoverageInfo (
    CoverageInfo
  , CoveragePerInput
  , findInCover
  , mergeSaveCover 
  , getCoverId
  , fromListCover
  , sizeCover
  ) where

import Data.Set    (Set, insert, size, member, map, fromList)
import Data.Hashable (hash)
import Echidna.ABI (SolCall)

type CoverageInfo = Set Int

getCoverId :: CoverageInfo -> String
getCoverId = show . hash . show

--type CoverageInfo = Set (Int, EVM.Concrete.Word, [EVM.Concrete.Word])
--type CoverageInfo = Set [(EVM.Concrete.Word, Int)]

--addCover :: VM -> CoverageInfo -> CoverageInfo
--addCover vm cov = insert (view pc $ view state vm, view gas $ view state vm , view stack $ view state vm ) cov 

-- coverage using storage
--addCover vm cov   = insert (map (\(x,y) -> (x, floor $ log $ fromInteger $ toInteger y)) $ Data.Map.toList $ view storage c) cov
--                    where c = snd $ last $ Data.Map.toList $ view contracts (view env vm)

type CoveragePerInput =  Set (CoverageInfo,[SolCall]) 

findInCover :: CoveragePerInput -> CoverageInfo -> Bool
findInCover cov icov = ( icov `member` Data.Set.map fst cov)

mergeSaveCover :: CoveragePerInput -> CoverageInfo -> [SolCall] -> IO CoveragePerInput
mergeSaveCover cov icov cs = if (findInCover cov icov) then return cov
                             else do
                                   --print $ size cov + 1
                                   --saveCalls cs dir (if reverted vm then "-rev" else "")
                                   return $ insert (icov, cs) cov

fromListCover :: [(CoverageInfo,[SolCall])] -> CoveragePerInput
fromListCover = fromList 

sizeCover :: CoveragePerInput -> Int
sizeCover = size
