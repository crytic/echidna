{-# LANGUAGE BangPatterns, FlexibleContexts, KindSignatures,
    LambdaCase, StrictData #-}

module Echidna.Exec (
    checkETest
  , CoverageInfo
  , CoverageRef
  , eCommand
  , eCommandCoverage
  , ePropertySeq
  , ePropertySeqCoverage
  , execCall
  , execCallCoverage
  , getCover
  , module Echidna.Internal.Runner
  ) where

import Control.Concurrent.MVar    (MVar, modifyMVar_)
import Control.Lens               ((^.), (.=), use)
import Control.Monad.Catch        (MonadCatch)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, StateT, evalState, evalStateT, execState, get, put, runState)
import Control.Monad.Reader       (MonadReader, ReaderT, runReaderT, ask)
import Data.IORef                 (IORef, modifyIORef', newIORef, readIORef)
import Data.List                  (intercalate, foldl')
import Data.Ord                   (comparing)
import Data.Set                   (Set, empty, insert, size, union)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Data.Vector.Generic        (maxIndexBy)

import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V

import Hedgehog
import Hedgehog.Gen               (choice, sequential)
import Hedgehog.Internal.State    (Action(..))
import Hedgehog.Internal.Property (PropertyConfig(..), mapConfig)
import Hedgehog.Range             (linear)

import EVM          (VM, VMResult(..), calldata, exec1, pc, result, stack, state)
import EVM.ABI      (AbiValue(..), abiCalldata, abiValueType, encodeAbiValue)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec)

import Echidna.ABI (SolCall, SolSignature, displayAbiCall, encodeSig, genInteractions, mutateCall)
import Echidna.Config (Config(..),testLimit,range)
import Echidna.Internal.Runner


--------------------------------------------------------------------
-- COVERAGE HANDLING

type CoverageInfo = (SolCall, Set Int)
type CoverageRef  = IORef CoverageInfo

getCover :: [CoverageInfo] -> IO [SolCall]
getCover [] = return []
getCover xs = setCover vs empty totalCoverage []
  where vs = V.fromList xs
        totalCoverage = size $ foldl' (\acc (_,c) -> union acc c) empty xs

setCover :: V.Vector CoverageInfo -> Set Int -> Int -> [SolCall] -> IO [SolCall]
setCover vs cov tot calls = do
    let i = maxIndexBy (\a b -> comparing (size . union cov) (snd a) (snd b)) vs
        s = vs V.! i
        c = union cov $ snd s
        newCalls = fst s : calls

    if size c == tot
      then return newCalls
      else do
      vs' <- V.unsafeThaw vs
      M.write vs' i mempty
      res <- V.unsafeFreeze vs'
      setCover res c tot newCalls
  

execCallUsing :: MonadState VM m => m VMResult -> SolCall -> m VMResult
execCallUsing m (t,vs) = do og <- get
                            cleanUp 
                            state . calldata .= cd
                            m >>= \case x@VMFailure{} -> put og >> return x
                                        x@VMSuccess{} -> return x
  where cd = B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ V.fromList vs
        cleanUp = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]


execCall :: MonadState VM m => SolCall -> m VMResult
execCall = execCallUsing exec


execCallCoverage :: (MonadState VM m, MonadReader CoverageRef m, MonadIO m) => SolCall -> m VMResult
execCallCoverage sol = execCallUsing (go empty) sol where
  go !c = use result >>= \case
    Just x -> do ref <- ask
                 liftIO $ modifyIORef' ref (const (sol, c))
                 return x
    _      -> do current <- use $ state . pc
                 S.state (runState exec1)
                 go $ insert current c

-------------------------------------------------------------------
-- Fuzzing and Hedgehog Init

checkETest :: VM -> Text -> Bool
checkETest v t = case evalState (execCall (t, [])) v of
  VMSuccess (B s) -> s == encodeAbiValue (AbiBool True)
  _               -> False


newtype VMState (v :: * -> *) =
  VMState VM
 
instance Show (VMState v) where
  show (VMState v) = "EVM state, current result: " ++ show (v ^. result)
 
newtype VMAction (v :: * -> *) = 
  Call SolCall
 
instance Show (VMAction v) where
  show (Call c) = displayAbiCall c

instance HTraversable VMAction where
  htraverse _ (Call b) = pure $ Call b


eCommandUsing :: (MonadGen n, MonadTest m, Typeable a)
              => n SolCall
              -> (VMAction Concrete -> m a)
              -> (VM -> Bool)
              -> Command n m VMState
eCommandUsing gen ex p = Command (\_ -> pure $ Call <$> gen) ex
  [ Ensure $ \_ (VMState v) _ _ -> assert $ p v
  , Update $ \(VMState v) (Call c) _ -> VMState $ execState (execCall c) v
  ]
  

eCommand :: (MonadGen n, MonadTest m) => n SolCall -> (VM -> Bool) -> Command n m VMState
eCommand = flip eCommandUsing (\ _ -> pure ())


eCommandCoverage :: (MonadGen n, MonadTest m, MonadState VM m, MonadReader CoverageRef m, MonadIO m)
                 => [SolCall] -> (VM -> Bool) -> [SolSignature] -> [Command n m VMState]
eCommandCoverage cov p ts = case cov of
  [] -> [eCommandUsing (genInteractions ts) (\(Call c) -> execCallCoverage c) p]
  xs -> map (\x -> eCommandUsing (choice [mutateCall x, genInteractions ts])
              (\(Call c) -> execCallCoverage c) p) xs


ePropertyUsing :: (MonadCatch m, MonadTest m, MonadReader Config n)
             => [Command Gen m VMState]
             -> (m () -> PropertyT IO ())
             -> VM             
             -> n Property
ePropertyUsing cs f v = do
  config <- ask
  return $ mapConfig (\x -> x {propertyTestLimit = fromIntegral $ config ^. testLimit}) . property $
    f . executeSequential (VMState v) =<< forAllWith printCallSeq
    (sequential (linear 1 (config ^. range)) (VMState v) cs)
  where printCallSeq = ("Call sequence: " ++) . intercalate "\n               " .
          map showCall . sequentialActions
        showCall (Action i _ _ _ _ _) = show i ++ ";"


ePropertySeq :: (MonadReader Config m)
             => (VM -> Bool)   -- Predicate to fuzz for violations of
             -> [SolSignature] -- Type signatures to fuzz
             -> VM             -- Initial state
             -> m Property
ePropertySeq p ts = ePropertyUsing [eCommand (genInteractions ts) p] id             


ePropertySeqCoverage :: (MonadReader Config m)
                     => [SolCall]
                     -> MVar [CoverageInfo]
                     -> (VM -> Bool)
                     -> [SolSignature]
                     -> VM
                     -> m Property
ePropertySeqCoverage calls cov p ts v = ePropertyUsing (eCommandCoverage calls p ts) writeCoverage v 
  where writeCoverage :: MonadIO m => ReaderT CoverageRef (StateT VM m) a -> m a
        writeCoverage m = do
          threadCovRef <- liftIO $ newIORef mempty
          let s = runReaderT m threadCovRef
          a         <- evalStateT s v
          threadCov <- liftIO $ readIORef threadCovRef
          liftIO $ modifyMVar_ cov (\xs -> pure $ threadCov:xs)
          return a
