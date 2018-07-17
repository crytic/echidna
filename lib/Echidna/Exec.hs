{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData #-}

module Echidna.Exec (
    ContractCov(..)
  , CoveragePoint(..)
  , CoverageReport(..)
  , byHashes
  , checkTest
  , checkBoolExpTest
  , checkRevertTest
  , checkTrueOrRevertTest
  , checkFalseOrRevertTest
  , CoverageInfo
  , CoverageRef
  , eCommand
  , eCommandCoverage
  , ePropertySeq
  , ePropertySeqCoverage
  , execCall
  , execCallCoverage
  , getCover
  , ppHashes
  , printResults
  , module Echidna.Internal.Runner
  ) where

import Control.Concurrent.MVar    (MVar, modifyMVar_)
import Control.Lens               ((&), (^.), (.=), use, view)
import Control.Monad.Catch        (MonadCatch)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, StateT, evalState, evalStateT, execState, get, put, runState)
import Control.Monad.Reader       (MonadReader, ReaderT, runReaderT, ask)
import Data.Aeson                 (ToJSON(..), encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable              (Foldable(..))
import Data.IORef                 (IORef, modifyIORef', newIORef, readIORef)
import Data.List                  (intercalate, foldl')
import Data.Map.Strict            (Map, insertWith, toAscList)
import Data.Maybe                 (fromMaybe)
import Data.Ord                   (comparing)
import Data.Set                   (Set, insert, singleton, size)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Data.Vector                (Vector, fromList)
import Data.Vector.Generic        (maximumBy)
import GHC.Generics

import qualified Control.Monad.State.Strict as S

import Hedgehog
import Hedgehog.Gen               (choice, sequential)
import Hedgehog.Internal.State    (Action(..))
import Hedgehog.Internal.Property (PropertyConfig(..), mapConfig)
import Hedgehog.Range             (linear)

import EVM
import EVM.ABI      (AbiValue(..), abiCalldata, abiValueType, encodeAbiValue)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec)
import EVM.Types    (W256)

import Echidna.ABI (SolCall, SolSignature, displayAbiCall, encodeSig, genInteractions, mutateCall)
import Echidna.Config (Config(..), testLimit, printCoverage, range, shrinkLimit)
import Echidna.Internal.Runner
import Echidna.Property (PropertyType(..))

--------------------------------------------------------------------
-- COVERAGE HANDLING

data CoveragePoint = C (Int, Int) W256 deriving Eq

instance Ord CoveragePoint where
  compare (C (_,i0) w0) (C (_,i1) w1) = case compare w0 w1 of EQ -> compare i0 i1
                                                              x  -> x

type CoverageInfo = (SolCall, Set CoveragePoint)
type CoverageRef  = IORef CoverageInfo

byHashes :: (Foldable t, Monoid (t CoveragePoint)) => t CoveragePoint -> Map W256 (Set (Int, Int))
byHashes = foldr (\(C i w) -> insertWith mappend w $ singleton i) mempty . toList

printResults :: (MonadIO m, MonadReader Config m) => Set CoveragePoint -> m ()
printResults ci = do liftIO (putStrLn $ "Coverage: " ++ show (size ci) ++ " unique arcs")
                     view printCoverage >>= \case True  -> liftIO . print . ppHashes $ byHashes ci
                                                  False -> pure ()

data ContractCov = ContractCov { hash :: String, arcs :: [(Int, Int)] } deriving (Show, Generic)
newtype CoverageReport = CoverageReport { coverage :: [ContractCov] } deriving (Show, Generic)

instance ToJSON ContractCov
instance ToJSON CoverageReport

ppHashes :: Map W256 (Set (Int, Int)) -> String
ppHashes = unpack . encode . toJSON . CoverageReport
  . map (\(h, is) -> ContractCov (show h) (toList is)) . toAscList

getCover :: (Foldable t, Monoid (t b)) => [(a, t b)] -> IO [a]
getCover [] = return []
getCover xs = setCover (fromList xs) mempty totalCoverage []
  where totalCoverage = length $ foldl' (\acc -> mappend acc . snd) mempty xs

setCover :: (Foldable t, Monoid (t b)) => Vector (a, t b) -> t b -> Int -> [a] -> IO [a]
setCover vs cov tot calls = best : calls & if length new == tot then return
                                                                else setCover vs new tot where
  (best, new) = mappend cov <$> maximumBy (comparing $ length . mappend cov . snd) vs
  

execCallUsing :: MonadState VM m => m VMResult -> SolCall -> m VMResult
execCallUsing m (t,vs) = do og <- get
                            cleanUp 
                            state . calldata .= cd
                            m >>= \case x@VMFailure{} -> put og >> return x
                                        x@VMSuccess{} -> return x
  where cd = B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs
        cleanUp = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]


execCall :: MonadState VM m => SolCall -> m VMResult
execCall = execCallUsing exec


execCallCoverage :: (MonadState VM m, MonadReader CoverageRef m, MonadIO m) => SolCall -> m VMResult
execCallCoverage sol = execCallUsing (go mempty) sol where
  go !c = use result >>= \case
    Just x -> do ref <- ask
                 liftIO $ modifyIORef' ref (const (sol, c))
                 return x
    _      -> do current <- use $ state . pc
                 ch      <- view codehash . fromMaybe (error "no current contract??") . currentContract <$> get 
                 S.state (runState exec1)
                 new     <- use $ state . pc
                 go $ insert (C (current, new) ch) c

-------------------------------------------------------------------
-- Fuzzing and Hedgehog Init

checkTest :: PropertyType -> VM -> Text -> Bool
checkTest ShouldReturnTrue             = checkBoolExpTest True
checkTest ShouldReturnFalse            = checkBoolExpTest False
checkTest ShouldRevert                 = checkRevertTest
checkTest ShouldReturnFalseRevert      = checkFalseOrRevertTest

checkBoolExpTest :: Bool -> VM -> Text -> Bool
checkBoolExpTest b v t = case evalState (execCall (t, [])) v of
  VMSuccess (B s) -> s == encodeAbiValue (AbiBool b)
  _               -> False

checkRevertTest :: VM -> Text -> Bool
checkRevertTest v t = case evalState (execCall (t, [])) v of
  (VMFailure Revert) -> True
  _                  -> False

checkTrueOrRevertTest :: VM -> Text -> Bool
checkTrueOrRevertTest v t = case evalState (execCall (t, [])) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool True)
  (VMFailure Revert) -> True
  _                  -> False

checkFalseOrRevertTest :: VM -> Text -> Bool
checkFalseOrRevertTest v t = case evalState (execCall (t, [])) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool False)
  (VMFailure Revert) -> True
  _                  -> False


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
                 => [SolCall] -> (VM -> Bool) -> [SolSignature] -> Config -> [Command n m VMState]
eCommandCoverage cov p ts conf = let useConf = flip runReaderT conf in case cov of
  [] -> [eCommandUsing (useConf $ genInteractions ts) (\(Call c) -> execCallCoverage c) p]
  xs -> map (\x -> eCommandUsing (choice $ useConf <$> [mutateCall x, genInteractions ts])
              (\(Call c) -> execCallCoverage c) p) xs

configProperty :: Config -> PropertyConfig -> PropertyConfig
configProperty config x = x { propertyTestLimit   = config ^. testLimit
                            , propertyShrinkLimit = config ^. shrinkLimit
                            }

ePropertyUsing :: (MonadCatch m, MonadTest m, MonadReader Config n)
             => [Command Gen m VMState]
             -> (m () -> PropertyT IO ())
             -> VM             
             -> n Property
ePropertyUsing cs f v = do
  config <- ask
  return $ mapConfig (configProperty config) . property $
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
ePropertySeq p ts vm = ask >>= \c -> ePropertyUsing [eCommand (runReaderT (genInteractions ts) c) p] id vm


ePropertySeqCoverage :: (MonadReader Config m)
                     => [SolCall]
                     -> MVar [CoverageInfo]
                     -> (VM -> Bool)
                     -> [SolSignature]
                     -> VM
                     -> m Property
ePropertySeqCoverage calls cov p ts v = ask >>= \c -> ePropertyUsing (eCommandCoverage calls p ts c) writeCoverage v 
  where writeCoverage :: MonadIO m => ReaderT CoverageRef (StateT VM m) a -> m a
        writeCoverage m = do
          threadCovRef <- liftIO $ newIORef mempty
          let s = runReaderT m threadCovRef
          a         <- evalStateT s v
          threadCov <- liftIO $ readIORef threadCovRef
          liftIO $ modifyMVar_ cov (\xs -> pure $ threadCov:xs)
          return a
