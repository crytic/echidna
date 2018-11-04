{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData, TupleSections #-}

module Echidna.Coverage (
  ePropertySeqCover
  ) where

import Control.Lens               (view, use, _1, _2, (&), (^.), (.=), (?~))
import Control.Monad              (forM, forM_)
import Control.Monad.State.Strict (MonadState, execState, execStateT, runState, get, put)
import Control.Monad.Catch        (MonadThrow, throwM)
import Control.Monad.Reader       (runReaderT)
import Control.Monad.IO.Class     (MonadIO)
import Data.Aeson.Types           (Value, toJSON, parseEither)
import Data.ByteString as BS      (concat)
import Data.Text                  (Text)
import Data.Map                   (Map)
import qualified Data.Map as Map
import Data.Set                   (Set, insert, size, elemAt, empty)

import Hedgehog
import Hedgehog.Gen               (choice)
import Hedgehog.Internal.Seed (seedValue)
import qualified Hedgehog.Internal.Seed as Seed

import EVM
import EVM.ABI (AbiType, AbiValue, encodeAbiValue)
import EVM.Concrete (Blob(..), w256)
import EVM.Exec     (exec, vmForEthrunCreation)
import EVM.Types (Addr(..))

import Echidna.ABI (SolCall, SolSignature, genTransactions, genAbiValueOfType, mutateCallSeq, fname, fargs, fvalue, fsender, parseAsType, reduceCallSeq)--, displayAbiSeq)
import Echidna.Config (Config(..), testLimit, range, outdir, initialValue, gasLimit, contractAddr)
import Echidna.Exec (encodeSolCall, sample, sampleDiff, reverted, fatal, checkProperties, filterProperties, minimizeTestcase)
import Echidna.Output (syncOutdir, updateOutdir)
import Echidna.Solidity (TestableContract, EchidnaException(ParseValueError), functions, config, ctorCode, constructor)
import Echidna.CoverageInfo (CoverageInfo, CoveragePerInput, mergeSaveCover)

import qualified Control.Monad.State.Strict as S

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
                            _1 . state . caller .= Addr (view fsender sc)
                            _1 . state . callvalue .= (fromIntegral $ view fvalue sc) 
                            x <- m
                            (_,cov) <- get
                            --traceM $ "cov=" ++ show (size cov)
                            case x of
                              VMSuccess _  -> return x
                              _            -> (put (og & result ?~ x, cov) >> return x) 

eConstructorGen :: (MonadGen m) => TestableContract -> Map Text AbiValue -> m [AbiValue]
eConstructorGen tcon abiParams = (flip runReaderT) (tcon ^. config) $ forM (tcon ^. constructor) $ \(name, typ) -> do
    case Map.lookup name abiParams of
        Nothing -> genAbiValueOfType typ
        Just v  -> choice [pure v, genAbiValueOfType typ]

parseAbiValue :: MonadThrow m => AbiType -> Value -> m AbiValue
parseAbiValue t v = case parseEither (uncurry parseAsType) (t, v) of
        Left err       -> throwM $ ParseValueError t v err
        Right abiValue -> return abiValue

eConstructorExec :: (MonadIO m) => Seed -> Size -> TestableContract -> Gen [AbiValue] -> m ([AbiValue], VM)
eConstructorExec seed ssize tcon gen = do
    let conf = tcon ^. config
    ctorArgs <- sample ssize seed gen >>= \case
        Nothing -> error "failed to generate constructor args"
        Just args -> return args
    (ctorArgs,) <$> execStateT
        (initializeVM conf)
        (vmForEthrunCreation $ (tcon ^. ctorCode) <> BS.concat (fmap encodeAbiValue ctorArgs))

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


ePropertyExec :: MonadIO m => Seed -> Size -> VM -> Gen [SolCall] -> [SolCall] -> m (CoverageInfo, VM, [SolCall])
ePropertyExec seed ssize ivm gen cs = do mcs <- sampleDiff ssize seed gen cs
                                         case mcs of 
                                           Nothing -> return (empty, ivm, []) 
                                           Just [] -> return (empty, ivm, []) 
                                           Just cs' -> do
                                                        let (cov, ecs, vm) = execCalls cs' ivm
                                                        return $ (cov, vm, ecs)

ePropertySeqCover :: [(Text, (VM -> Bool))] -> TestableContract -> IO ()
ePropertySeqCover ps tcon = do
                              print (toInteger $ c ^. testLimit) 
                              cov <- syncOutdir (c ^. outdir) mempty
                              ePropertySeqCover' (toInteger $ c ^. testLimit) cov ps tcon
                             where c = tcon ^. config 

chooseFromSeed :: Seed -> Set a -> a
chooseFromSeed seed set = elemAt (fromEnum $ seedValue seed `mod` ssize ) set
                           where ssize = fromIntegral $ size set
lastIter :: Integer -> Bool
lastIter i    = i == 0

everyXIter :: Integer -> Integer -> Bool
everyXIter x n = (n `mod` x) == 0

ePropertySeqCover' :: Integer -> CoveragePerInput -> [(Text, (VM -> Bool))] -> TestableContract -> IO ()
ePropertySeqCover'   _ _ [] _                            = return ()
ePropertySeqCover'   n _ _  _      | lastIter n          = return ()
ePropertySeqCover'   n cov ps tcon | everyXIter 123456 n  = do
                                                            cov' <- syncOutdir (tcon ^. config ^. outdir) cov
                                                            ePropertySeqCover' (n-1) cov' ps tcon

ePropertySeqCover'   n cov ps tcon = do 
                                          --traceShowM $ "n=" ++ show n
                                          seed <- Seed.random
                                          (_, cs, vs) <- return $ if (null cov) then (empty, [], Map.empty) else chooseFromSeed seed cov
                                          let ctor = tcon ^. constructor
                                          let conMap = Map.fromList ctor
                                          abiParams <- sequence $ Map.intersectionWith parseAbiValue conMap vs
                                          let ctorGen = eConstructorGen tcon abiParams
                                          let funcGen = ePropertySeqMutate ts cs ssize c
                                          (abiVals, ivm) <- eConstructorExec seed tsize tcon ctorGen
                                          let cvs' = Map.fromList $ zip (map fst ctor) (map toJSON abiVals)
                                          --print (tsize, ssize) 
                                          (icov, vm, cs') <- ePropertyExec seed tsize ivm funcGen cs
                                          updateOutdir cov (icov, cs', cvs') (c ^. outdir)
                                          cov' <- mergeSaveCover cov icov cs' cvs' --(c ^. outdir)
                                          --putStrLn $ displayAbiSeq cs
                                          --putStrLn "+++++++"
                                          --putStrLn $ displayAbiSeq cs'
                                          --putStrLn "------"
                                          --print cov'
                                          if (reverted vm) 
                                          then ePropertySeqCover' (n-1) cov' ps tcon
                                          else do
                                                (tp,fp) <- return $ checkProperties ps vm
                                                forM_ (filterProperties ps fp) (minimizeTestcase cs ivm tcon)
                                                ePropertySeqCover' (n-1) cov' (filterProperties ps tp) tcon
                                         where tsize  = fromInteger $ n `mod` 100
                                               ssize  = fromInteger $ max 1 $ n `mod` (toInteger (c ^. range))
                                               ts     = view functions tcon
                                               c      = view config tcon



ePropertySeqMutate :: MonadGen m => [SolSignature] -> [SolCall] -> Int -> Config -> m [SolCall]
ePropertySeqMutate ts cs ssize c = if (null cs) 
                                   then ePropertyGen ts ssize c 
                                   else choice [useConf $ mutateCallSeq ts cs, ePropertyGen ts ssize c, reduceCallSeq cs]      
                                   where useConf = flip runReaderT c 
