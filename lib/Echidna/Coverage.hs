{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData #-}

module Echidna.Coverage (
  ePropertySeqCover
  ) where

import Control.Lens               (view, use, _1, _2, (&), (^.), (.=), (?~))
import Control.Monad              (forM_)
import Control.Monad.State.Strict (MonadState, execState, get, put)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.IO.Class     (MonadIO)
import Data.Text                  (Text)
import Data.Set                   (Set, insert, size, member, elemAt, empty)
import qualified Data.Set         (map{-, null-})

import Hedgehog
import Hedgehog.Gen               (choice)
import Hedgehog.Internal.Seed (seedValue)
import qualified Hedgehog.Internal.Seed as Seed

import EVM

import Echidna.ABI (SolCall, SolSignature, genTransactions, mutateCallSeq, fname, fargs, fvalue, fsender, displayAbiSeq)
import Echidna.Config (Config(..), testLimit, range, outdir)
import Echidna.Exec (encodeSolCall, sample, reverted, fatal, checkProperties, filterProperties, minimizeTestcase)
import Echidna.Output (saveCalls)
import Echidna.Solidity (TestableContract, initialVM, functions, config)

import qualified Control.Monad.State.Strict as S

type CoverageInfo = Set Int
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



mergeSaveCover :: CoveragePerInput -> CoverageInfo -> [SolCall] -> VM -> Maybe String -> IO (Set (CoverageInfo, [SolCall]))
mergeSaveCover cov icov cs vm dir = if (findInCover cov icov) then return cov
                                    else do
                                       --print $ size cov + 1
                                       saveCalls cs dir (if reverted vm then "-rev" else "")
                                       return $ insert (icov, cs) cov

-----------------------------------------
-- Echidna exec with coverage

mexec :: MonadState (VM, CoverageInfo) m => m VMResult
mexec = do current <- use $ _1 . state . pc
           cov <- use _2 
           S.state (updateState (insert current cov))
           use (_1 . result) >>= \case
                                  Just x -> return x
                                  _      -> mexec


updateState :: CoverageInfo -> (VM, CoverageInfo) -> ((), (VM, CoverageInfo))
updateState c (vm,_) = (x,(vm',c))
                   where (x,vm') = S.runState exec1 vm 

execCalls :: [SolCall] -> VM -> (CoverageInfo, [SolCall], VM)
execCalls cs ivm = foldr f (mempty, [], ivm) $ cs
                   where f c (cov, ecs, vm) = let (vm',cov') = execState (execCallUsing c mexec) (vm,cov) in
                                                  if (reverted vm || fatal vm) then (cov, ecs, vm)
                                                                               else (cov', c:ecs, vm') 

cleanUpAfterTransaction :: MonadState (VM, CoverageInfo) m => m ()
cleanUpAfterTransaction = sequence_ [(_1 . result .= Nothing), (_1 . state . pc .= 0) , (_1 . state . stack .= mempty)] 


execCallUsing :: MonadState (VM, CoverageInfo) m => SolCall -> m VMResult -> m VMResult
execCallUsing sc m =     do (og,_) <- get
                            cleanUpAfterTransaction
                            _1 . state . calldata .= encodeSolCall (view fname sc) (view fargs sc)
                            _1 . state . caller .= view fsender sc
                            _1 . state . callvalue .= (fromIntegral $ view fvalue sc) 
                            x <- m
                            (_,cov) <- get
                            case x of
                              VMSuccess _  -> return x
                              _            -> (put (og & result ?~ x, cov) >> return x) 
 
ePropertyGen :: MonadGen m => [SolSignature] -> Int -> Config -> m [SolCall]
ePropertyGen ts n c = runReaderT (genTransactions n ts) c 


ePropertyExec :: MonadIO m => Seed -> Size -> VM -> Gen [SolCall] -> m (CoverageInfo, VM, [SolCall])
ePropertyExec seed ssize ivm gen = do mcs <- sample ssize seed gen
                                      case mcs of 
                                       Nothing -> return (empty, ivm, []) 
                                       Just [] -> return (empty, ivm, []) 
                                       Just cs -> do
                                                  let (cov, ecs, vm) = execCalls cs ivm
                                                  --if null cov then error ("invalid coverage information using " ++ show ecs) else return ()
                                                  return $ (cov, vm, ecs)

ePropertySeqCover :: [(Text, (VM -> Bool))] -> TestableContract -> IO ()
ePropertySeqCover ps tcon =  ePropertySeqCover' (toInteger $ tcon ^. config ^. testLimit) empty ps tcon


chooseFromSeed :: Seed -> Set a -> a
chooseFromSeed seed set = elemAt (fromEnum $ seedValue seed `mod` ssize ) set
                           where ssize = fromIntegral $ size set

ePropertySeqCover' :: Integer -> CoveragePerInput -> [(Text, (VM -> Bool))] -> TestableContract -> IO ()
ePropertySeqCover'   _ _ [] _              = return ()
ePropertySeqCover'   n _ _  _      | n == 0 = return ()
{-
ePropertySeqCover'   n cov ps tcon | n >= (toInteger (c ^. testLimit))-1000 = do
                                                       seed <- Seed.random
                                                       (icov, vm, cs) <- ePropertyExec seed tsize ivm gen
                                                       cov' <- mergeSaveCover cov icov cs vm (c ^. outdir)
                                                       --cov' <- return $ insert (icov, cs) cov 
                                                       --putStrLn $ displayAbiSeq cs
                                                       --print cov'
                                                       --print "-----"
                                                       if (reverted vm) 
                                                       then ePropertySeqCover' (n-1) cov' ps tcon
                                                       else do
                                                           (tp,fp) <- return $ checkProperties ps vm
                                                           forM_ (filterProperties ps fp) (minimizeTestcase cs tcon)
                                                           ePropertySeqCover' (n-1) cov' (filterProperties ps tp) tcon
                                                        where tsize  = fromInteger $ n `mod` 100
                                                              ssize  = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))
                                                              gen    = ePropertyGen ts ssize c
                                                              ivm    = view initialVM tcon
                                                              ts     = view functions tcon
                                                              c      = view config tcon

-}

ePropertySeqCover'   n cov ps tcon = do 
                                          seed <- Seed.random
                                          cs' <- return $ if (null cov) then [] else snd $ chooseFromSeed seed cov
                                          let gen = ePropertySeqMutate ts cs' ssize c 
                                          (icov, vm, cs) <- ePropertyExec seed tsize ivm gen
                                          cov' <- mergeSaveCover cov icov cs vm (c ^. outdir)
                                          --putStrLn $ displayAbiSeq cs
                                          --print cov'
                                          --print "----"
                                          if (reverted vm) 
                                          then ePropertySeqCover' (n-1) cov' ps tcon
                                          else do
                                                (tp,fp) <- return $ checkProperties ps vm
                                                forM_ (filterProperties ps fp) (minimizeTestcase cs tcon)
                                                ePropertySeqCover' (n-1) cov' (filterProperties ps tp) tcon
                                         where tsize  = fromInteger $ n `mod` 100
                                               ssize  = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))
                                               ivm    = view initialVM tcon
                                               ts     = view functions tcon
                                               c      = view config tcon



ePropertySeqMutate :: MonadGen m => [SolSignature] -> [SolCall] -> Int -> Config -> m [SolCall]
ePropertySeqMutate ts cs ssize c = if (null cs) 
                                   then ePropertyGen ts ssize c 
                                   else choice [useConf $ mutateCallSeq ts cs, ePropertyGen ts ssize c]      
                                   where useConf = flip runReaderT c 
