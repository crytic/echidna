{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData #-}

module Echidna.Coverage (
  --  CoverageInfo
  --, CoverageRef
  --, CoverageReport(..)
  --, eCommandCoverage
  ePropertySeqCover
  --, getCoverageReport
  ) where

import Control.Lens               (view ,(&), (^.), (.=), (?~))
import Control.Monad              (forM_)
import Control.Monad.State.Strict (MonadState, execState, get, put)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.IO.Class     (MonadIO)
import Data.Text                  (Text)
import Data.Set                   (Set, insert, size, member, elemAt, empty)
import qualified Data.Set         (map, null)

import Crypto.Hash.SHA256         (hash)
import System.Directory           (doesFileExist, createDirectoryIfMissing)

import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Base16 as B16 (encode) 

import Hedgehog
import Hedgehog.Gen               (choice)
import Hedgehog.Internal.Seed (seedValue)
import qualified Hedgehog.Internal.Seed as Seed

import EVM
import EVM.Concrete (Word)
import EVM.Exec     (exec)

import Echidna.ABI (SolCall, SolSignature, displayAbiSeq, genTransactions, mutateCallSeq)
import Echidna.Config (Config(..), testLimit, range, outdir)
import Echidna.Exec (encodeSolCall, cleanUpAfterTransaction, sample, reverted, checkProperties, filterProperties, processResult)

type CoverageInfo = Set (Int, EVM.Concrete.Word, [EVM.Concrete.Word])
--type CoverageInfo = Set [(EVM.Concrete.Word, Int)]

addCover :: VM -> CoverageInfo -> CoverageInfo
addCover vm cov = insert (view pc $ view state vm, view gas $ view state vm , view stack $ view state vm ) cov 

-- coverage using storage
--addCover vm cov   = insert (map (\(x,y) -> (x, floor $ log $ fromInteger $ toInteger y)) $ Data.Map.toList $ view storage c) cov
--                    where c = snd $ last $ Data.Map.toList $ view contracts (view env vm)

type CoveragePerInput =  Set (CoverageInfo,[SolCall]) 

findInCover :: CoveragePerInput -> CoverageInfo -> Bool
findInCover cov icov = ( icov `member` Data.Set.map fst cov)

mergeSaveCover :: CoveragePerInput -> CoverageInfo -> [SolCall] -> Maybe String -> IO (Set (CoverageInfo, [SolCall]))
mergeSaveCover cov icov cs dir = if ( findInCover cov icov) then return cov
                                 else do
                                       print $ size cov + 1
                                       saveCalls cs dir
                                       return $ insert (icov, cs) cov 

saveCalls :: [SolCall] -> Maybe String-> IO ()
saveCalls _ Nothing     = return ()
saveCalls cs (Just dir) = do
                            let content = displayAbiSeq cs ++ "\n"
                            let filename = dir ++ "/" ++ (hashString $ content)
                            createDirectoryIfMissing False dir
                            b <- doesFileExist filename
                            if (not b) then (writeFile filename $ content) else return ()

-----------------------------------------
-- Echidna exec with coverage


execCalls :: [SolCall] -> VM -> (CoverageInfo, VM)
execCalls cs ivm = foldr f (mempty, ivm) cs
                   where f c (cov, vm) = let vm' = execState (execCallUsing c exec) vm in
                                         if (reverted vm) then (mempty, vm)
                                                          else (addCover vm' cov, vm') 

execCallUsing :: MonadState VM m => SolCall -> m VMResult -> m VMResult
execCallUsing (t,vs) m = do og <- get
                            cleanUpAfterTransaction
                            state . calldata .= encodeSolCall t vs
                            x <- m
                            case x of
                              VMSuccess _  -> return x
                              _            -> (put (og & result ?~ x) >> return x) 
 
ePropertyGen :: MonadGen m => [SolSignature] -> Int -> Config -> m [SolCall]
ePropertyGen ts n c = runReaderT (genTransactions n ts) c 


ePropertyExec :: MonadIO m => Seed -> Size -> VM -> Gen [SolCall] -> m (CoverageInfo, VM, [SolCall])
ePropertyExec seed ssize ivm gen = do mcs <- sample ssize seed gen
                                      case mcs of 
                                       Nothing -> return (empty, ivm, []) 
                                       Just cs -> do
                                                  let (cov, vm) = execCalls cs ivm 
                                                  return $ (cov, vm, cs)

ePropertySeqCover :: [(Text, (VM -> Bool))] -> [SolSignature] -> VM -> Config -> IO ()
ePropertySeqCover ps ts ivm c =  ePropertySeqCover' (toInteger $ c ^. testLimit) empty ps ts ivm c


chooseFromSeed :: Seed -> Set a -> a
chooseFromSeed seed set = elemAt ( (fromEnum $ seedValue seed `mod` 9223372036854775807) `mod` ssize ) set
                           where ssize = size set

ePropertySeqCover' :: Integer -> CoveragePerInput -> [(Text, (VM -> Bool))] -> [SolSignature] -> VM -> Config -> IO ()
ePropertySeqCover'   _ _ [] _  _   _          = return ()
ePropertySeqCover'   n _ _  _  _   _ | n == 0 = return ()
ePropertySeqCover'   n cov ps ts ivm c | Data.Set.null cov = do
                                                       seed <- Seed.random
                                                       (icov, vm, cs) <- ePropertyExec seed tsize ivm gen
                                                       if (reverted vm) 
                                                       then ePropertySeqCover' (n-1) cov ps ts ivm c
                                                       else do
                                                           cov' <- return $ insert (icov, cs) cov 
                                                           (tp,fp) <- return $ checkProperties ps vm
                                                           forM_ (filterProperties ps fp) (processResult cs gen tsize ivm c)
                                                           ePropertySeqCover' (n-1) cov' (filterProperties ps tp) ts ivm c
                                                        where tsize  = fromInteger $ n `mod` 100
                                                              ssize  = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))
                                                              gen    = ePropertyGen ts ssize c

ePropertySeqCover'   n cov ps ts ivm c = do 
                                          seed <- Seed.random
                                          cs' <- return $ snd $ chooseFromSeed seed cov
                                          let gen = if (null cs') 
                                                    then ePropertyGen ts ssize c 
                                                    else ePropertySeqMutate ts cs' ssize c 
                                          (icov, vm, cs) <- ePropertyExec seed tsize ivm gen
                                          if (reverted vm) 
                                          then ePropertySeqCover' (n-1) cov ps ts ivm c
                                          else do
                                                cov' <- mergeSaveCover cov icov cs  (c ^. outdir)
                                                (tp,fp) <- return $ checkProperties ps vm
                                                forM_ (filterProperties ps fp) (processResult cs gen tsize ivm c)
                                                ePropertySeqCover' (n-1) cov' (filterProperties ps tp) ts ivm c
                                         where tsize  = fromInteger $ n `mod` 100
                                               ssize  = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))


ePropertySeqMutate :: MonadGen m => [SolSignature] -> [SolCall] -> Int -> Config -> m [SolCall]
ePropertySeqMutate ts cs ssize c = choice [useConf $ mutateCallSeq cs, ePropertyGen ts ssize c]      
                                   where useConf = flip runReaderT c 

hashString :: String -> String
hashString = BS.unpack . B16.encode . hash . BS.pack 
