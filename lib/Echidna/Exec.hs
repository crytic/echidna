{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData #-}

module Echidna.Exec (
    checkTest
  , checkBoolExpTest
  , checkRevertTest
  , checkTrueOrRevertTest
  , checkFalseOrRevertTest
  , ePropertySeq
  , execCalls
  , execCall
  , execCallUsing
  , encodeSolCall
  , cleanUpAfterTransaction
  , sample 
  , reverted
  , checkProperties
  , filterProperties
  , processResult
  ) where

import Control.Lens               (view, (&), (^.), (.=), (?~))
import Control.Monad              (forM_)
import Control.Monad.State.Strict (MonadState, evalState, execState, get, put)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Maybe  (runMaybeT)
import Data.Text                  (Text)
import Data.Vector                (fromList)
import Data.Functor.Identity      (runIdentity)
import Data.Maybe                 (catMaybes)

import Hedgehog
import Hedgehog.Gen               (scale) --, list, small)
import Hedgehog.Internal.Gen      (runGenT)
import Hedgehog.Internal.Tree (Tree(..), Node(..))
import qualified Hedgehog.Internal.Seed as Seed

import EVM
import EVM.ABI      (AbiValue(..), abiCalldata, abiValueType, encodeAbiValue)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec)
import EVM.Types    (Addr)

import Echidna.ABI (SolCall(..), SolSignature, displayAbiSeq, encodeSig, genTransactions, fargs, fname, fsender, fvalue)
import Echidna.Config (Config(..), testLimit, range, shrinkLimit)
import Echidna.Property (PropertyType(..))

-------------------------------------------------------------------
-- Fuzzing and Hedgehog Init

sample :: MonadIO m => Size -> Seed -> Gen a -> m (Maybe a)
sample size seed gen =
  liftIO $
    let
      loop n =
        if n <= 0 then
          pure $ Nothing 
        else do
          case runIdentity . runMaybeT . runTree $ runGenT size seed gen of
            Nothing ->
              loop (n - 1)
            Just x ->
              pure $ Just $ nodeValue x
    in
      loop (1 :: Int)

{-
fatalFailures :: VM -> Bool
fatalFailures vm = case (vm ^. result) of
  (Just (VMFailure (Query _)))                    -> True
  (Just (VMFailure (UnrecognizedOpcode _)))       -> True
  (Just (VMFailure StackUnderrun))                -> True
  (Just (VMFailure BadJumpDestination))           -> True
  (Just (VMFailure StackLimitExceeded))           -> True
  (Just (VMFailure IllegalOverflow))              -> True
  _                                               -> False
-}

reverted :: VM -> Bool
reverted vm = case (vm ^. result) of
  (Just (VMFailure Revert))                    -> True
  Nothing                                      -> False
  _                                            -> False

--extractVMResult Nothing = exec 
--extractVMResult (Just vmr) = return $ vmr

execCalls :: [SolCall] -> VM -> VM
execCalls cs ivm = foldr f ivm cs
                   where f c vm = if (reverted vm) then vm 
                                  else (execState (execCallUsing c exec)) vm 

execCall :: MonadState VM m => SolCall -> m VMResult
execCall c = execCallUsing c exec

execCallUsing :: MonadState VM m => SolCall -> m VMResult -> m VMResult
execCallUsing sc m =     do og <- get
                            cleanUpAfterTransaction
                            state . calldata .= encodeSolCall (view fname sc) (view fargs sc)
                            state . caller .= view fsender sc
                            state . callvalue .= (fromIntegral $ view fvalue sc)
                            x <- m
                            case x of
                              VMSuccess _  -> return x
                              _            -> (put (og & result ?~ x) >> return x) 
 
encodeSolCall :: Text -> [AbiValue] -> Blob        
encodeSolCall t vs =  B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs

cleanUpAfterTransaction :: MonadState VM m => m ()
cleanUpAfterTransaction = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]

ePropertyGen :: MonadGen m => [SolSignature] -> Int -> Config -> m [SolCall]
ePropertyGen ts n c = runReaderT (genTransactions n ts) c 

ePropertyExec :: MonadIO m => Seed -> Size -> VM -> Gen [SolCall] -> m (VM, [SolCall])
ePropertyExec seed size ivm gen = do mcs <- sample size seed gen
                                     case mcs of 
                                       Nothing -> return (ivm, []) 
                                       Just cs -> return (execCalls cs ivm, cs) 

ePropertySeq :: [(Text, (VM -> Bool))] -> [SolSignature] -> VM -> Config -> IO ()
ePropertySeq ps ts ivm c =  ePropertySeq' (toInteger $ c ^. testLimit) ps ts ivm c


ePropertySeq' :: Integer -> [(Text, (VM -> Bool))] -> [SolSignature] -> VM -> Config -> IO ()
ePropertySeq'   _ [] _  _   _          = return ()
ePropertySeq'   n _  _  _   _ | n == 0 = return () 
ePropertySeq'   n ps ts ivm c          = do 
                                          seed <- Seed.random
                                          (vm, cs) <- ePropertyExec seed tsize ivm gen
                                          (tp,fp) <- return $ checkProperties ps vm 
                                          forM_ (filterProperties ps fp) (processResult cs gen tsize ivm c)
                                          ePropertySeq' (n-1) (filterProperties ps tp) ts ivm c
                                         where tsize  = fromInteger $ n `mod` 100
                                               ssize  = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))
                                               gen    = ePropertyGen ts ssize c

--printCache vm c = if reverted vm then print c else print c

checkProperties ::  [(Text, (VM -> Bool))] -> VM -> ([Text],[Text])
checkProperties ps vm = if reverted vm then (map fst ps, [])
                        else (map fst $ filter snd bs, map fst $ filter (not . snd) bs)
                        where bs = map (\(t,p) -> (t, p vm)) ps

filterProperties :: [(Text, (VM -> Bool))] -> [Text] -> [(Text, (VM -> Bool))] 
filterProperties ps ts = filter (\(t,_) -> t `elem` ts) ps 

processResult :: [SolCall] -> Gen [SolCall] -> Size -> VM -> Config -> (Text, VM -> Bool) -> IO () 
processResult cs gen size ivm c (t,p) = do
                                          putStrLn "Failed property:"
                                          print t
                                          putStrLn "Original input:"
                                          putStrLn $ displayAbiSeq cs
                                          putStrLn "Shrinking:"
                                          xs <- sequence $ shrink p c ivm size gen
                                          let shrinked = fst $ findSmaller cs $ catMaybes xs 
                                          putStrLn $ displayAbiSeq shrinked
                                          putStrLn "Done!"

shrink :: MonadIO m => (VM -> Bool) -> Config -> VM -> Size -> Gen [SolCall] -> [m (Maybe ([SolCall], Int))]
shrink p c ivm size gen = shrink' p (fromIntegral $ c ^. shrinkLimit) ivm size gen 

shrink' :: MonadIO m => (VM -> Bool) -> Int -> VM -> Size -> Gen [SolCall] -> [m (Maybe ([SolCall], Int))]
shrink' p n ivm size gen = map f $ take n $ iterate (scale (\x -> round (fromIntegral x * 0.99 :: Double))) gen
  where f sgen = do seed <- Seed.random
                    (vm, cs) <- ePropertyExec seed size ivm sgen
                    --print $ cs
                    if (not $ p vm) then return $ Just (cs, length cs)
                                    else return Nothing 

findSmaller :: [SolCall] -> [([SolCall],Int)] -> ([SolCall], Int)
findSmaller ics = foldr (\(cs, s) (cs', s') -> if s < s' then (cs, s) else (cs', s')) (ics, length ics) 

--fmapNodes f size seed =
--    fmap f . runIdentity . runMaybeT . runTree . runGenT size seed . Hedgehog.Internal.Gen.lift

checkTest :: PropertyType -> VM -> Text -> Bool
checkTest ShouldReturnTrue             = checkBoolExpTest True
checkTest ShouldReturnFalse            = checkBoolExpTest False
checkTest ShouldRevert                 = checkRevertTest
checkTest ShouldReturnFalseRevert      = checkFalseOrRevertTest

defaultSender = 0x00a329c0648769a73afac7f9381e08fb43dbea70

checkBoolExpTest :: Bool -> VM -> Text -> Bool
checkBoolExpTest b v t = case evalState (execCall (SolCall t [] defaultSender 0)) v of
  VMSuccess (B s) -> s == encodeAbiValue (AbiBool b)
  _               -> False

checkRevertTest :: VM -> Text -> Bool
checkRevertTest v t = case evalState (execCall (SolCall t [] defaultSender 0)) v of
  (VMFailure Revert) -> True
  _                  -> False

checkTrueOrRevertTest :: VM -> Text -> Bool
checkTrueOrRevertTest v t = case evalState (execCall (SolCall t [] defaultSender 0)) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool True)
  (VMFailure Revert) -> True
  _                  -> False

checkFalseOrRevertTest :: VM -> Text -> Bool
checkFalseOrRevertTest v t = case evalState (execCall (SolCall t [] defaultSender 0)) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool False)
  (VMFailure Revert) -> True
  _                  -> False
