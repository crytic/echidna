module Echidna.Worker.ConstraintDiscovery (runConstraintDiscoveryWorker) where

import Control.Concurrent.STM
  ( TChan, atomically, dupTChan, orElse, readTChan, writeTChan )
import Control.Monad (when)
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
  , maxContextsPerSignature
  )
import Echidna.Types.InterWorker
  ( AgentId(..), ConstraintRequest(..), FuzzerCmd(..), Message(..)
  , WrappedMessage(..)
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
  requestChan <- liftIO $ atomically $ dupTChan env.bus
  eventChan <- liftIO $ atomically $ dupTChan env.eventQueue
  onReady
  (_, finalState) <- flip runStateT (initialWorkerState { workerId }) $ do
    callback
    liftIO $ serveRequests env workerId contract requestChan eventChan
    callback
  pure (ConstraintDiscoveryDone, finalState)

serveRequests
  :: Echidna.Env
  -> Int
  -> SolcContract
  -> TChan WrappedMessage
  -> TChan (LocalTime, CampaignEvent)
  -> IO ()
serveRequests env workerId contract requestChan eventChan
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
      { maxIter = Just 1
      , maxConcreteIter = Just 1
      , askSmtIters = 1
      , loopHeuristic = StackBased
      }
    hevmConfig = defaultConfig
      { maxWidth = 4
      , maxDepth = Just 8
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
        `orElse` (Left <$> readTChan requestChan)
      case input of
        Right (_, WorkerEvent _ FuzzWorker (WorkerStopped _))
          | workersAlive == 1 -> pure ()
          | otherwise -> loop solvers cache reported (workersAlive - 1)
        Left (WrappedMessage (FuzzerId requester) (ToConstraint request)) -> do
          (bounds, cache') <- case Map.lookup request.key cache of
            Just cached -> pure (cached, cache)
            Nothing -> do
              let cachedContexts = Map.size $
                    Map.filterWithKey
                      (\key _ -> key.signature == request.key.signature)
                      cache
              if cachedContexts >= maxContextsPerSignature
                then pure (Map.empty, cache)
                else do
                  discovered <- discover solvers request
                  pure (discovered, Map.insert request.key discovered cache)
          reported' <- reportNewBounds reported request.key.signature bounds
          liftIO $ respond requester request.key bounds
          loop solvers cache' reported' workersAlive
        _ -> loop solvers cache reported workersAlive

    discover solvers request =
      case Map.lookup request.key.signature methods of
        Just method | suitableForConstraintDiscovery method -> do
          discoverMethodBounds method request.vm request.targetTx
            iterConfig solvers rpcInfo env.fetchSession
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

    respond requester key bounds = atomically $ writeTChan env.bus $ WrappedMessage
      { from = ConstraintDiscoveryId
      , content = ToFuzzer requester (SetArgumentBounds key bounds)
      }

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
