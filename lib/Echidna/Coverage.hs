{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData #-}

module Echidna.Coverage (
    CoverageInfo
  , CoverageRef
  , CoverageReport(..)
  , eCommandCoverage
  , ePropertySeqCoverage
  , execCallCoverage
  , getCover
  , getCoverageReport
  , saveCalls
  , module Echidna.Internal.Runner
  , module Echidna.Internal.JsonRunner
  ) where

import Control.DeepSeq            (force)
import Control.Concurrent.MVar    (MVar, modifyMVar_)
import Control.Lens               ((&), use)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, runState)
import Control.Monad.Reader       (MonadReader, ReaderT, runReaderT, ask)
import Data.Aeson                 (ToJSON(..), encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Foldable              (Foldable(..), foldl')
import Data.IORef                 (IORef, modifyIORef', newIORef, readIORef)
import Data.Ord                   (comparing)
import Data.Set                   (Set, insert, size)
import Data.Vector                (Vector, fromList)
import Data.Vector.Generic        (maximumBy)
import GHC.Generics
import Crypto.Hash.SHA256         (hashlazy, hash)
import System.Directory           (doesFileExist)

import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Base16 as B16 (encode) 

import Hedgehog
import Hedgehog.Gen               (choice)

import EVM

import Echidna.ABI (SolCall, SolSignature, genInteractions, mutateCall, displayAbiCall)
import Echidna.Config (Config(..))
import Echidna.Internal.Runner
import Echidna.Internal.JsonRunner
import Echidna.Exec

-----------------------------------------
-- Coverage data types and printing

type CoverageInfo = (SolCall, Set Int)
type CoverageRef  = IORef CoverageInfo

data CoverageReport = CoverageReport {coverage :: Int} deriving (Show,Generic)

instance ToJSON CoverageReport

getCoverageReport :: Set Int -> String
getCoverageReport = unpack . encode . toJSON . CoverageReport . size

-----------------------------------------
-- Set cover algo

getCover :: (Foldable t, Monoid (t b)) => [(a, t b)] -> [a]
getCover [] = []
getCover xs = setCover (fromList xs) mempty totalCoverage []
  where totalCoverage = length $ foldl' (\acc -> mappend acc . snd) mempty xs

setCover :: (Foldable t, Monoid (t b)) => Vector (a, t b) -> t b -> Int -> [a] -> [a]
setCover vs cov tot calls = best : calls & if length new == tot then id
                                                                else setCover vs new tot where
  (best, new) = mappend cov <$> maximumBy (comparing $ length . mappend cov . snd) vs

-----------------------------------------
-- Echidna exec with coverage
  
execCallCoverage :: (MonadState VM m, MonadReader CoverageRef m, MonadIO m) => SolCall -> m VMResult
execCallCoverage sol = execCallUsing (go mempty) sol where
  go !c = use result >>= \case
    Just x -> do ref <- ask
                 liftIO $ modifyIORef' ref (const (sol, c))
                 return x
    _      -> do current <- use $ state . pc
                 S.state (runState exec1)
                 go . force $ insert current c

eCommandCoverage :: (MonadGen n, MonadTest m, MonadState VM m, MonadReader CoverageRef m, MonadIO m)
                 => [SolCall] -> (VM -> Bool) -> [SolSignature] -> Config -> [Command n m VMState]
eCommandCoverage cov p ts conf = let useConf = flip runReaderT conf in case cov of
  [] -> [eCommandUsing (useConf $ genInteractions ts) (\(Call c) -> execCallCoverage c) p]
  xs -> map (\x -> eCommandUsing (choice $ useConf <$> [mutateCall x, genInteractions ts])
              (\(Call c) -> execCallCoverage c) p) xs

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


hashString :: String -> String
hashString = BS.unpack . B16.encode . hash . BS.pack 

saveCalls :: [CoverageInfo] -> IO ()
saveCalls cs = mapM_ f cs
  where f (c,m) = do
          let filename = "txs/" ++ (hashString $ show m)
          b <- doesFileExist filename
          if (not b) then (writeFile filename $ displayAbiCall c ++ "\n") else return ()
