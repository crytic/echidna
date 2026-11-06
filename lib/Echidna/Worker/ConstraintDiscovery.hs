module Echidna.Worker.ConstraintDiscovery (runConstraintDiscoveryWorker) where

import Control.Concurrent.STM
  ( TChan, atomically, dupTChan, orElse, readTChan, writeTChan )
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (MonadReader, asks, liftIO, runReaderT)
import Control.Monad.State.Strict (MonadIO, StateT, runStateT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (LocalTime)

import EVM.Dapp (DappInfo(..))
import EVM.Effects (App, Config(..), Env(..), defaultConfig, defaultEnv)
import EVM.Fetch (RpcInfo(..))
import EVM.Solidity (Method(..), SolcContract(..))
import EVM.Solvers (SolverGroup, withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic(..))

import Echidna.Solidity (chooseContract)
import Echidna.SymExec.Bounds (discoverMethodBounds, suitableForConstraintDiscovery)
import Echidna.SymExec.Common (rpcFetcher)
import Echidna.Types.Campaign
  ( CampaignConf(..), WorkerState(..), getNFuzzWorkers, initialWorkerState )
import Echidna.Types.Config qualified as Echidna
import Echidna.Types.Constraint
  ( ArgumentBounds, ConstraintKey(..), NumericBounds(..)
  , maxRecentContextsPerSignature
  )
import Echidna.Types.InterWorker
  ( AgentId(..), ConstraintRequest(..), FuzzerCmd(..), Message(..)
  , WrappedMessage(..), takeConstraintRequest
  )
import Echidna.Types.Worker
  ( CampaignEvent(..), WorkerEvent(..), WorkerStopReason(..), WorkerType(..) )
import Echidna.Worker (pushCampaignEvent)

-- | Serve demand-driven discovery requests with one solver. Fuzzers never
-- wait for this worker: a miss is computed here and sent back asynchronously,
-- while repeated requests for the same concrete context are answered from
-- cache.
runConstraintDiscoveryWorker
  :: (MonadIO m, MonadThrow m, MonadReader Echidna.Env m)
  => StateT WorkerState m ()
  -> m ()
  -> Int
  -> Maybe T.Text
  -> m (WorkerStopReason, WorkerState)
runConstraintDiscoveryWorker callback onReady workerId contractName = do
  env <- asks id
  contract <- chooseContract (Map.elems env.dapp.solcByName) contractName
  eventChan <- liftIO $ atomically $ dupTChan env.eventQueue
  onReady
  (_, finalState) <- flip runStateT (initialWorkerState { workerId }) $ do
    callback
    liftIO $ serveRequests env workerId contract eventChan
    callback
  pure (ConstraintDiscoveryDone, finalState)

serveRequests
  :: Echidna.Env
  -> Int
  -> SolcContract
  -> TChan (LocalTime, CampaignEvent)
  -> IO ()
serveRequests env workerId contract eventChan
  | fuzzWorkers == 0 = pure ()
  | otherwise = flip runReaderT runtimeEnv $
      withSolvers conf.campaignConf.symExecSMTSolver 1 (Just 1) solverMemory $ \solvers ->
        loop solvers Map.empty Map.empty fuzzWorkers
  where
    conf = env.cfg
    fuzzWorkers = getNFuzzWorkers conf.campaignConf
    methods = Map.fromList
      [ (method.methodSignature, method)
      | method <- Map.elems contract.abiMap
      ]
    rpcInfo = RpcInfo (rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock))
    iterConfig = IterConfig
      { maxIter = Just 2
      , maxConcreteIter = Just 8
      , askSmtIters = 1
      , loopHeuristic = StackBased
      }
    hevmConfig = defaultConfig
      { maxWidth = 4
      , maxDepth = Just 16
      , maxBufSize = 12
      , maxDynSize = 16
      , promiseNoReent = False
      , onlyDeployed = True
      , debug = False
      , dumpQueries = False
      , abstractArith = False
      }
    runtimeEnv = defaultEnv { config = hevmConfig }
    solverMemory = min 1024 conf.campaignConf.symExecSolverMemory
    discoveryTimeout = max 1 $ min 2 conf.campaignConf.symExecTimeout

    loop
      :: App m
      => SolverGroup
      -> Map ConstraintKey ArgumentBounds
      -> Map T.Text [ArgumentBounds]
      -> Int
      -> m ()
    loop solvers cache reported workersAlive = do
      input <- liftIO $ atomically $
        (Right <$> readTChan eventChan)
        `orElse` (Left <$> takeConstraintRequest env.constraintQueue)
      case input of
        Right (_, WorkerEvent _ FuzzWorker (WorkerStopped _))
          | workersAlive == 1 -> pure ()
          | otherwise -> loop solvers cache reported (workersAlive - 1)
        Left (requester, request) -> do
          (bounds, cache') <- case Map.lookup request.key cache of
            Just cached -> pure (cached, cache)
            Nothing -> do
              discovered <- discover solvers request
              pure (discovered, rememberDiscovery request.key discovered cache)
          reported' <- reportNewBounds reported request.key.signature bounds
          liftIO $ respond fuzzWorkers requester request bounds
          loop solvers cache' reported' workersAlive
        Right _ -> loop solvers cache reported workersAlive

    discover solvers request =
      case Map.lookup request.key.signature methods of
        Just method | suitableForConstraintDiscovery method ->
          discoverMethodBounds method request.vm request.targetTx
            iterConfig discoveryTimeout solvers rpcInfo env.fetchSession
        _ -> pure Map.empty

    reportNewBounds reported signature bounds = do
      let redundant = bounds `elem` Map.findWithDefault [] signature reported
          useful = not $ Map.null bounds
      when (useful && not redundant) $ liftIO $ pushCampaignEvent env $
        WorkerEvent workerId ConstraintDiscoveryWorker . Log $
          "New argument bounds discovered for " <> T.unpack signature
          <> ": " <> renderBounds bounds
      pure $ if useful && not redundant
        then Map.insertWith (<>) signature [bounds] reported
        else reported

    respond workerCount requester request bounds = atomically $ do
      let recipients
            | Map.null bounds = [requester]
            | otherwise = [0 .. workerCount - 1]
      mapM_ (\recipient -> writeTChan env.bus $ WrappedMessage
        { from = ConstraintDiscoveryId
        , content = ToFuzzer recipient (SetArgumentBounds request.key bounds)
        }) recipients
      unless (Map.null bounds) $ writeTChan env.bus $ WrappedMessage
        { from = ConstraintDiscoveryId
        , content = ToFuzzer requester
            (ProbeArgumentBounds request.key request.targetTx bounds)
        }

-- Retain a small rolling sample for reuse without letting early corpus states
-- suppress discovery in states reached later by fuzzing.
rememberDiscovery
  :: ConstraintKey
  -> ArgumentBounds
  -> Map ConstraintKey ArgumentBounds
  -> Map ConstraintKey ArgumentBounds
rememberDiscovery key bounds cache
  | Map.member key cache = Map.insert key bounds cache
  | otherwise = Map.insert key bounds trimmed
  where
    sameSignature = Map.filterWithKey
      (\cachedKey _ -> cachedKey.signature == key.signature)
      cache
    trimmed
      | Map.size sameSignature < maxRecentContextsPerSignature = cache
      | otherwise = case Map.lookupMin sameSignature of
          Just (previous, _) -> Map.delete previous cache
          Nothing -> cache

renderBounds :: Map Int NumericBounds -> String
renderBounds = T.unpack . T.intercalate ", " . fmap render . Map.toAscList
  where
    render (argument, bounds) =
      "arg" <> T.pack (show $ argument + 1) <> interval bounds

    interval NumericBounds{lowerBound = Just lower, upperBound = Just upper}
      | lower == upper = " == " <> showT lower
      | otherwise = " in [" <> showT lower <> ", " <> showT upper <> "]"
    interval NumericBounds{lowerBound = Just lower} = " >= " <> showT lower
    interval NumericBounds{upperBound = Just upper} = " <= " <> showT upper
    interval _ = ""

    showT = T.pack . show
