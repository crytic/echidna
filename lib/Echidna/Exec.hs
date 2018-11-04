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
  , sampleDiff 
  , reverted
  , fatal
  , checkProperties
  , filterProperties
  , minimizeTestcase
  ) where

import Control.Lens               (view, use, (&), (^.), (.=), (?~))
import Control.Monad              (forM_)
import Control.Monad.State.Strict (MonadState, evalState, execState, runState, execStateT, get, put)
import qualified Control.Monad.State.Strict as S
import Control.Monad.Reader       (runReaderT)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Maybe  (runMaybeT)
import Data.ByteString as BS      (concat)
import Data.Text                  (Text)
import Data.Vector                (fromList)
import Data.Functor.Identity      (runIdentity)
import Data.Maybe                 (catMaybes)
import Data.Monoid                ((<>))
import Hedgehog
import Hedgehog.Internal.Gen      (runGenT)
import Hedgehog.Internal.Tree (Tree(..), Node(..))
import qualified Hedgehog.Internal.Seed as Seed

import EVM
import EVM.ABI      (AbiValue(..), AbiType, abiCalldata, abiValueType, encodeAbiValue)
import EVM.Concrete (Blob(..), w256, forceConcreteBlob)
import EVM.Exec     (exec, vmForEthrunCreation)
import EVM.Types    (Addr(..))

import Echidna.ABI (SolCall(..), SolSignature, encodeSig, genTransactions, genAbiValueOfType, fargs, fname, fsender, fvalue, reduceCallSeq) --, displayAbiSeq)
import Echidna.Config (Config(..), testLimit, range, shrinkLimit, outputJson, outputRawTxs, initialValue, gasLimit, contractAddr)
import Echidna.Property (PropertyType(..))
import Echidna.Output (reportPassedTest, reportFailedTest)
import Echidna.Solidity (TestableContract, ctorCode, functions, events, config, constructor)
import Echidna.Event (extractSeqLog, Events)
--import Echidna.Shrinking (minimizeTestcase)
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

sampleDiff :: (Eq a, MonadIO m) => Size -> Seed -> Gen a -> a -> m (Maybe a)
sampleDiff size seed gen a =
  liftIO $
    let
      loop n =
        if n <= 0 then
          pure $ Nothing 
        else do
          case runIdentity . runMaybeT . runTree $ runGenT size seed gen of
            Nothing ->
              loop (n - 1)
            Just x | (nodeValue x) == a ->
              loop (n - 1) 
            Just x ->
              pure $ Just $ nodeValue x
    in
      loop (20 :: Int)


fatal :: VM -> Bool
fatal vm = case (vm ^. result) of
  --(Just (VMFailure (Query _)))                    -> True
  --(Just (VMFailure (UnrecognizedOpcode _)))     -> True
  (Just (VMFailure StackUnderrun))                -> True
  (Just (VMFailure BadJumpDestination))           -> True
  (Just (VMFailure StackLimitExceeded))           -> True
  (Just (VMFailure IllegalOverflow))              -> True
  _                                               -> False

reverted :: VM -> Bool
reverted vm = case (vm ^. result) of
  (Just (VMFailure Revert))                    -> True
  (Just (VMFailure (UnrecognizedOpcode _)))    -> True
  Nothing                                      -> False
  _                                            -> False

execCalls :: [SolCall] -> VM -> ([SolCall], VM)
execCalls cs ivm = foldr f ([], ivm) cs
                   where f c (ecs, vm) = if (reverted vm || fatal vm) then (ecs, vm)
                                         else (c:ecs, execState (execCallUsing c exec) vm)

execCall :: MonadState VM m => SolCall -> m VMResult
execCall c = execCallUsing c exec

execCallUsing :: MonadState VM m => SolCall -> m VMResult -> m VMResult
execCallUsing sc m =     do og <- get
                            cleanUpAfterTransaction
                            state . calldata .= encodeSolCall (view fname sc) (view fargs sc)
                            state . caller .= (Addr (view fsender sc))
                            state . callvalue .= (fromIntegral $ view fvalue sc)
                            x <- m
                            case x of
                              VMSuccess _  -> return x
                              _            -> (put (og & result ?~ x) >> return x) 
 
encodeSolCall :: Text -> [AbiValue] -> Blob        
encodeSolCall t vs =  B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs

cleanUpAfterTransaction :: MonadState VM m => m ()
cleanUpAfterTransaction = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]

eConstructorGen :: MonadGen m => [AbiType] -> Config -> m [AbiValue]
eConstructorGen ts = runReaderT (mapM genAbiValueOfType ts)

eConstructorExec :: (MonadIO m) => Seed -> Size -> TestableContract -> Gen [AbiValue] -> m VM
eConstructorExec seed size tcon gen = do
    let conf = tcon ^. config
    ctorArgs <- sample size seed gen >>= \case
        Nothing -> error "failed to generate constructor args"
        Just args -> return args
    let encodedArgs = BS.concat $ map encodeAbiValue ctorArgs
    execStateT
        (initializeVM conf)
        (vmForEthrunCreation $ (tcon ^. ctorCode) <> encodedArgs)

initializeVM :: (MonadState VM m) => Config -> m VM
initializeVM conf = do
        state . callvalue .= (fromIntegral $ conf ^. initialValue)
        exec >>= \case
            VMFailure _      -> get
            VMSuccess (B bc) -> do
                let l = S.state . runState
                l $ replaceCodeOfSelf bc
                c <- use $ state . contract
                l $ resetState
                state . gas .= (w256 $ conf ^. gasLimit)
                state . contract .= (conf ^. contractAddr)
                state . codeContract .= (conf ^. contractAddr)
                l $ loadContract c
                get

ePropertyGen :: MonadGen m => [SolSignature] -> Int -> Config -> m [SolCall]
ePropertyGen ts n c = runReaderT (genTransactions n ts) c 

ePropertyExec :: MonadIO m => Seed -> Size -> VM -> Gen [SolCall] -> m (VM, [SolCall])
ePropertyExec seed size ivm gen = do mcs <- sample size seed gen
                                     case mcs of 
                                       Nothing -> return (ivm, []) 
                                       Just cs -> do
                                                  let (ecs, vm) = execCalls cs ivm
                                                  return $ (vm, ecs) --take idx $ reverse cs)

ePropertySeq :: [(Text, (VM -> Bool))] -> TestableContract -> IO ()
ePropertySeq ps tcon =  ePropertySeq' (toInteger $ tcon ^. config ^. testLimit) ps tcon

ePropertySeq' :: Integer -> [(Text, (VM -> Bool))] -> TestableContract -> IO ()
ePropertySeq'   _ []   _           = return ()
ePropertySeq'   n ps tcon | n == 0 = forM_ (map fst ps) (reportPassedTest (tcon ^. config ^. outputJson))
ePropertySeq'   n ps tcon          = do 
                                          seed <- Seed.random
                                          ivm <- eConstructorExec seed tsize tcon ctorGen
                                          (vm, cs) <- ePropertyExec seed tsize ivm funcGen
                                          --putStrLn ( show $ view traces vm)
                                          if  (tcon ^. config ^. outputRawTxs) then ( 
                                               print $ map (\x -> forceConcreteBlob $ encodeSolCall (view fname x) (view fargs x)) cs
                                               ) 
                                          else return () 
                                          --let es = extractSeqLog (view events tcon) (view logs vm) 
                                          if (reverted vm) then ePropertySeq' (n-1) ps tcon
                                          else do 
                                                (tp,fp) <- return $ checkProperties ps vm 
                                                forM_ (filterProperties ps fp) (minimizeTestcase cs ivm tcon)
                                                ePropertySeq' (n-1) (filterProperties ps tp) tcon
                                         where c =  tcon ^. config 
                                               tsize   = fromInteger $ n `mod` 100
                                               ssize   = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))
                                               funcGen = ePropertyGen ts ssize c
                                               ctorGen = eConstructorGen (map snd $ tcon ^. constructor) c
                                               ts      = view functions tcon

checkProperties ::  [(Text, (VM -> Bool))] -> VM -> ([Text],[Text])
checkProperties ps vm = if fatal vm then ([], map fst ps)
                        else (map fst $ filter snd bs, map fst $ filter (not . snd) bs)
                        where bs = map (\(t,p) -> (t, p vm)) ps

filterProperties :: [(Text, (VM -> Bool))] -> [Text] -> [(Text, (VM -> Bool))] 
filterProperties ps ts = filter (\(t,_) -> t `elem` ts) ps 

--fmapNodes f size seed =
--    fmap f . runIdentity . runMaybeT . runTree . runGenT size seed . Hedgehog.Internal.Gen.lift

checkTest :: PropertyType -> Addr -> VM -> Text -> Bool
checkTest ShouldReturnTrue             = checkBoolExpTest True
checkTest ShouldReturnFalse            = checkBoolExpTest False
checkTest ShouldRevert                 = checkRevertTest
checkTest ShouldReturnFalseRevert      = checkFalseOrRevertTest

checkBoolExpTest ::  Bool -> Addr -> VM -> Text -> Bool
checkBoolExpTest b addr v t = case evalState (execCall (SolCall t [] (addressWord160 addr) 0)) v of
  VMSuccess (B s) -> s == encodeAbiValue (AbiBool b)
  _               -> False

checkRevertTest :: Addr -> VM -> Text -> Bool
checkRevertTest addr v t = case evalState (execCall (SolCall t [] (addressWord160 addr) 0)) v of
  (VMFailure Revert) -> True
  _                  -> False

checkTrueOrRevertTest :: Addr -> VM -> Text -> Bool
checkTrueOrRevertTest addr v t = case evalState (execCall (SolCall t [] (addressWord160 addr) 0)) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool True)
  (VMFailure Revert) -> True
  _                  -> False

checkFalseOrRevertTest :: Addr -> VM -> Text -> Bool
checkFalseOrRevertTest addr v t = case evalState (execCall (SolCall t [] (addressWord160 addr) 0)) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool False)
  (VMFailure Revert) -> True
  _                  -> False


-- Shrinking functions. TODO: move to another module (e.g. Echidna.Shrinking)


minimizeTestcase :: [SolCall] -> VM -> TestableContract -> (Text, VM -> Bool) -> IO () 
minimizeTestcase cs ivm tcon (t,p) = do
                                     rcs <- minimizeTestcase' cs ivm (conf ^. shrinkLimit) (t,p)
                                     rev <- extractEvents rcs ivm tcon
                                     ev <- extractEvents cs ivm tcon
                                     reportFailedTest (conf ^. outputJson) t cs ev rcs rev
                                    where conf = (tcon ^. config)



extractEvents :: [SolCall] -> VM -> TestableContract -> IO Events
extractEvents cs ivm tcon = do
    let (_, vm) = execCalls cs ivm
    return $ extractSeqLog (tcon ^. events) (vm ^. logs)

minimizeTestcase' :: [SolCall] -> VM -> Int -> (Text, VM -> Bool) -> IO [SolCall]
minimizeTestcase' cs  _  0   _   = return cs
minimizeTestcase' cs ivm n (t,p) = do
                                     xs <- sequence $ shrink p ivm cs
                                     let rcs = fst $ findSmaller cs $ catMaybes xs
                                     minimizeTestcase' rcs ivm (n-1) (t,p) 
                                          

shrink :: MonadIO m => (VM -> Bool) -> VM -> [SolCall] -> [m (Maybe ([SolCall], Int))]
shrink p ivm cs = map f $ zip (repeat $ reduceCallSeq cs) [0 .. 10]
  where f (sgen,size) = do seed <- Seed.random
                           (vm, cs') <- ePropertyExec seed size ivm sgen
                           if (not $ p vm) then return $ Just (cs', length cs')
                                           else return Nothing 

findSmaller :: [SolCall] -> [([SolCall],Int)] -> ([SolCall], Int)
findSmaller ics = foldr (\(cs, s) (cs', s') -> if s < s' then (cs, s) else (cs', s')) (ics, length ics) 
