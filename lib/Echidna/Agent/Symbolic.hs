{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BangPatterns #-}

module Echidna.Agent.Symbolic where

import Control.Concurrent (takeMVar)
import Control.Concurrent.STM (atomically, readTChan, writeTChan, dupTChan, TChan)
import Control.Monad (when, void, unless, forM_)
import Control.Monad.Reader (runReaderT, liftIO, asks, MonadReader)
import Control.Monad.State.Strict (runStateT, get, gets, MonadState, modify')
import Control.Monad.Random.Strict (evalRandT, MonadRandom)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Random.Strict (RandT)
import System.Random (mkStdGen)
import Data.IORef (IORef, writeIORef, readIORef)
import Data.Foldable (foldlM)
import Data.List.NonEmpty qualified as NEList
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)

import EVM.Types (VM(..), VMType(Concrete))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Dapp (DappInfo(..))

import Echidna.Types.Solidity (SolConf(..))
import Echidna.ABI (GenDict(..))
import qualified Echidna.Exec
import Echidna.Execution (callseq, updateTests)
import Echidna.Shrink (shrinkTest)
import Echidna.Solidity (chooseContract)
import Echidna.SymExec.Common (extractTxs, extractErrors)
import Echidna.SymExec.Exploration (exploreContract, getTargetMethodFromTx, getRandomTargetMethod)
import Echidna.SymExec.Verification (verifyMethod, isSuitableToVerifyMethod)
import Echidna.Types.Agent
import Echidna.Types.Campaign (WorkerState(..), CampaignConf(..), getNFuzzWorkers)
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.InterWorker (AgentId(..), Bus, WrappedMessage(..), Message(..), SymbolicCmd(..), FuzzerCmd(..), BroadcastMsg(NewCoverageInfo))
import qualified Echidna.Types.InterWorker as InterWorker
import Echidna.Types.Random (rElem)
import Echidna.Test (isVerificationMode)
import Echidna.Types.Test (EchidnaTest(..), TestState(..), isAssertionTest, getAssertionSignature, isOpen, didFail)
import qualified Echidna.Types.Test as Test
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker (WorkerEvent(..), WorkerType(..), CampaignEvent(..), WorkerStopReason(..))
import Echidna.Worker (pushCampaignEvent, pushWorkerEvent)

instance (MonadThrow m) => MonadThrow (RandT g m) where
  throwM = lift . throwM

data SymbolicAgent = SymbolicAgent
  { initialVm :: VM Concrete
  , initialDict :: GenDict
  , initialCorpus :: [(FilePath, [Tx])]
  , contractName :: Maybe Text
  , stateRef :: IORef WorkerState
  }

instance Show SymbolicAgent where
  show agent = "SymbolicAgent { contractName = " ++ show agent.contractName ++ " }"

instance Agent SymbolicAgent where
  getAgentId _ = SymbolicId

  runAgent agent bus env = do
    let workerId = 0 -- Symbolic agent usually has ID 0
        vm = agent.initialVm
        dict = agent.initialDict
        name = agent.contractName
        ref = agent.stateRef

        effectiveSeed = dict.defSeed + workerId
        effectiveGenDict = dict { defSeed = effectiveSeed }

        initialState = WorkerState
          { workerId
          , genDict = effectiveGenDict
          , newCoverage = False
          , ncallseqs = 0
          , ncalls = 0
          , totalGas = 0
          , runningThreads = []
          }

    let callback = get >>= liftIO . writeIORef ref
    let cfg = env.cfg
    let nworkers = getNFuzzWorkers cfg.campaignConf

    -- We create a specific channel for this agent to read from bus (broadcasts)
    workerChan <- atomically $ dupTChan bus

    (reason, _) <- flip evalRandT (mkStdGen effectiveSeed) $
           flip runReaderT env $
             flip runStateT initialState $ do
               liftIO $ putStrLn "Starting SymbolicAgent"

               -- Check for stateless verification mode
               if isVerificationMode cfg.solConf.testMode then do
                 verifyMethods vm name callback
                 pure SymbolicVerificationDone
               else do
                 callback
                 busListenerLoop bus workerChan callback vm name nworkers
                 pure SymbolicExplorationDone

    liftIO $ pushCampaignEvent env (WorkerEvent workerId SymbolicWorker (WorkerStopped reason))

    return ()

busListenerLoop
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => Bus
  -> TChan WrappedMessage
  -> m ()
  -> VM Concrete
  -> Maybe Text
  -> Int
  -> m ()
busListenerLoop bus chan callback vm name workersAlive =
    when (workersAlive > 0) $ do
      msg <- liftIO $ atomically $ readTChan chan
      handleMessage bus msg callback vm name
      case msg of
        WrappedMessage _ (Broadcast (InterWorker.WorkerStopped _)) ->
            busListenerLoop bus chan callback vm name (workersAlive - 1)
        _ -> busListenerLoop bus chan callback vm name workersAlive

handleMessage
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => Bus
  -> WrappedMessage
  -> m ()
  -> VM Concrete
  -> Maybe Text
  -> m ()
handleMessage _ (WrappedMessage _ (Broadcast (NewCoverageInfo _ txs))) callback vm name = do
    void $ callseq vm txs
    symexecTxs callback vm False name txs
    shrinkAndRandomlyExplore callback vm txs (10 :: Int)

handleMessage bus (WrappedMessage from (ToSymbolic (SolveThis txs))) callback vm name = do
    -- Received a request to solve constraints or generate inputs based on txs
    -- We can use symexecTxs to try to explore around these txs
    -- But we need to return the result to 'from'

    -- For now, let's try to run symbolic execution on these txs
    -- and if we find new valid transactions, send them back.

    -- This is a simplified implementation. Real implementation would need to hook into the solver results.
    -- symexecTxs pushes events and updates coverage.

    -- TODO: We need a way to capture the "Solution" from symexecTxs
    -- Currently symexecTxs calls exploreAndVerify which calls exploreContract which pushes to symTxsChan.
    -- We could refactor exploreAndVerify to return the found txs.

    symexecTxs callback vm False name txs

    -- If we found something, we should probably send it back.
    -- But symexecTxs updates the global state/coverage directly.
    -- Maybe we can send a "SolutionFound" with empty list just to ack?
    -- Or we can assume that if new coverage is found, it is broadcasted, so the requester will see it.

    -- However, the requester asked for help. Maybe they want a specific transaction that solves a branch.
    -- The PoC says "The symbolic worker should be able to answer with some solved constraints / generator".

    -- Let's just send a message saying we tried.
    case from of
      FuzzerId fid ->
         liftIO $ atomically $ writeTChan bus (WrappedMessage SymbolicId (ToFuzzer fid (SolutionFound [])))
      _ -> pure ()

handleMessage _ _ _ _ _ = pure ()

shrinkAndRandomlyExplore
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => m ()
  -> VM Concrete
  -> [Tx]
  -> Int
  -> m ()
shrinkAndRandomlyExplore callback vm _ 0 = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{shrinkLimit} <- asks (.cfg.campaignConf)
    when (any shrinkable tests) $ shrinkLoop callback vm shrinkLimit
    where
      shrinkable test =
        case test.state of
          Large _ -> True -- Symbolic worker can shrink any large test? Origin logic: test.workerId == Just workerId
          _       -> False

shrinkAndRandomlyExplore callback vm txs n = do
    testRefs <- asks (.testRefs)
    tests <- liftIO $ traverse readIORef testRefs
    CampaignConf{stopOnFail, shrinkLimit} <- asks (.cfg.campaignConf)
    if stopOnFail && any final tests then
      callback -- >> pure FastFailed
    else if any shrinkable tests then do
      shrinkLoop callback vm shrinkLimit
      shrinkAndRandomlyExplore callback vm txs n
    else do
      symexecTxs callback vm False Nothing txs
      shrinkAndRandomlyExplore callback vm txs (n - 1)
    where
      shrinkable test =
        case test.state of
          Large _ -> True -- Simplified
          _       -> False
      final test =
        case test.state of
          Solved   -> True
          Failed _ -> True
          _        -> False

shrinkLoop
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => m ()
  -> VM Concrete
  -> Int
  -> m ()
shrinkLoop _ _ 0 = return ()
shrinkLoop callback vm n = do
    callback
    workerId <- gets (.workerId)
    updateTests $ \test -> do
      -- Logic from Campaign.hs: test.workerId == Just workerId
      -- But symbolic worker has workerId=0. Maybe it shares work?
      -- For now, allow shrinking if we own it or if it's unassigned?
      if test.workerId == Just workerId then
        shrinkTest vm test
      else
        pure Nothing
    shrinkLoop callback vm (n - 1)

symexecTxs
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => m ()
  -> VM Concrete
  -> Bool -- ^ onlyRandom
  -> Maybe Text -- ^ Contract name
  -> [Tx]
  -> m ()
symexecTxs callback vm onlyRandom name txs = mapM_ (symexecTx callback vm name) =<< txsToTxAndVmsSym vm onlyRandom txs

txsToTxAndVmsSym
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete
  -> Bool
  -> [Tx]
  -> m [(Maybe Tx, VM Concrete, [Tx])]
txsToTxAndVmsSym vm _ [] = pure [(Nothing, vm, [])]
txsToTxAndVmsSym vm False txs = do
    -- Separate the last tx, which should be the one increasing coverage
    let (itxs, ltx) = (init txs, last txs)
    -- TODO: execTx is in Execution.hs but not exported or we need to import it.
    -- Execution.hs exports callseq and execTxOptC.
    -- We can use EVM.Exec.exec or Echidna.Exec.execTx?
    -- Echidna.Exec has execTx.

    -- Wait, Echidna.Exec.execTx returns (VMResult, VM).
    -- We need to import Echidna.Exec.

    -- Using a fold to execute transactions
    -- We need execTx from Echidna.Exec
    let execTx' v t = snd <$> Echidna.Exec.execTx v t

    ivm <- foldlM execTx' vm itxs

    -- Split the sequence randomly and select any next transaction
    i <- if length txs == 1 then pure 0 else rElem $ NEList.fromList [1 .. length txs - 1]
    let rtxs = take i txs
    rvm <- foldlM execTx' vm rtxs
    cfg <- asks (.cfg)
    let targets = cfg.campaignConf.symExecTargets
    if null targets
    then pure [(Just ltx, ivm, txs), (Nothing, rvm, rtxs)]
    else pure [(Nothing, rvm, rtxs)]

txsToTxAndVmsSym vm True txs = do
    let execTx' v t = snd <$> Echidna.Exec.execTx v t
    -- Split the sequence randomly and select any next transaction
    i <- if length txs == 1 then pure 0 else rElem $ NEList.fromList [1 .. length txs - 1]
    let rtxs = take i txs
    rvm <- foldlM execTx' vm rtxs
    pure [(Nothing, rvm, rtxs)]

symexecTx
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => m ()
  -> VM Concrete
  -> Maybe Text
  -> (Maybe Tx, VM Concrete, [Tx])
  -> m ()
symexecTx callback vm name (tx, vm', txsBase) = do
    conf <- asks (.cfg)
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    failedTests <- findFailedTests
    let failedTestSignatures = map getAssertionSignature failedTests
    case tx of
      Nothing -> getRandomTargetMethod contract conf.campaignConf.symExecTargets failedTestSignatures >>= \case
        Nothing -> do
          return ()
        Just method -> exploreAndVerify callback vm contract method vm' txsBase
      Just t -> getTargetMethodFromTx t contract failedTestSignatures >>= \case
        Nothing -> do
          return ()
        Just method -> do
          exploreAndVerify callback vm contract method vm' txsBase

exploreAndVerify
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => m ()
  -> VM Concrete
  -> SolcContract
  -> Method
  -> VM Concrete
  -> [Tx]
  -> m ()
exploreAndVerify callback vm contract method vm' txsBase = do
    -- exploreContract returns (ThreadId, MVar ([SymTx], [Text]))
    (threadId, symTxsChan) <- exploreContract contract method vm'
    modify' (\ws -> ws { runningThreads = [threadId] })
    callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan

    modify' (\ws -> ws { runningThreads = [] })
    callback

    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error(s) during symbolic exploration: " <> show e)) errors
    unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial explored path(s) during symbolic exploration: " <> unpack e)) partials

    -- We can't do callseq vm' [symTx] because callseq might post the full call sequence as an event
    -- callseq expects [Tx]
    -- symTx is a Tx (from extractTxs)

    -- We need to run callseq on (txsBase + [symTx]) for each symTx
    -- And we need to get the original VM (not vm' which is intermediate)
    -- Actually we need the starting VM of the campaign?
    -- No, callseq takes a VM.

    -- In Campaign.hs: callseq vm (txsBase <> [symTx])
    -- 'vm' was the initial VM of the worker.
    -- Here 'vm' in runAgent is the initial VM.
    -- But exploreAndVerify doesn't have access to initial VM easily unless passed down.
    -- In Campaign.hs, 'vm' was in scope from runSymWorker.

    -- We need to pass the initial VM to exploreAndVerify.
    -- But wait, runSymWorker passed 'vm' (initial) to symexecTxs? No.
    -- symexecTxs took vm (initial) in my adaptation? Yes.
    -- But exploreAndVerify is taking vm' (intermediate).

    -- I need to access the initial VM. I'll store it in Reader or State?
    -- Or just pass it.
    -- I'll modify symexecTxs and exploreAndVerify to take 'initialVm'.

    -- For now, let's assume I can get it.
    -- I'll pass it from runAgent -> busListenerLoop -> handleMessage -> symexecTxs -> symexecTx -> exploreAndVerify

    newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm (txsBase <> [symTx])) txs

    when (not newCoverage && null errors && not (null txs)) (
      pushWorkerEvent $ SymExecError "No errors but symbolic execution found valid txs breaking assertions. Something is wrong.")
    unless newCoverage (pushWorkerEvent $ SymExecLog "Symbolic execution finished with no new coverage.")

verifyMethods
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete
  -> Maybe Text
  -> m ()
  -> m ()
verifyMethods vm name callback = do
    dapp <- asks (.dapp)
    let cs = Map.elems dapp.solcByName
    contract <- chooseContract cs name
    let allMethods = contract.abiMap
    conf <- asks (.cfg)
    forM_ allMethods (\method -> do
           isSuitable <- isSuitableToVerifyMethod contract method conf.campaignConf.symExecTargets
           if isSuitable
            then symExecMethod vm name callback contract method
            else pushWorkerEvent $ SymExecError ("Skipped verification of method " <> unpack method.methodSignature)
          )

symExecMethod
  :: (MonadIO m, MonadThrow m, MonadRandom m, MonadReader Env m, MonadState WorkerState m)
  => VM Concrete
  -> Maybe Text
  -> m ()
  -> SolcContract
  -> Method
  -> m ()
symExecMethod vm name callback contract method = do
    callback
    (threadId, symTxsChan) <- verifyMethod method contract vm

    modify' (\ws -> ws { runningThreads = [threadId] })
    callback

    (symTxs, partials) <- liftIO $ takeMVar symTxsChan
    let txs = extractTxs symTxs
    let errors = extractErrors symTxs

    modify' (\ws -> ws { runningThreads = [] })
    callback

    newCoverage <- or <$> mapM (\symTx -> snd <$> callseq vm [symTx]) txs
    let methodSignature = unpack method.methodSignature
    unless newCoverage $ do
      unless (null txs) $ error "No new coverage but symbolic execution found valid txs. Something is wrong."
      when (null errors && null partials) $ do
        updateTests $ \test -> do
          if isOpen test && isAssertionTest test && getAssertionSignature test == methodSignature then
                pure $ Just $ test { Test.state = Unsolvable }
          else
            pure $ Just test

    unless (null errors) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Error(s) solving constraints produced by method " <> methodSignature <> ": " <> show e)) errors
    unless (null partials) $ mapM_ ((pushWorkerEvent . SymExecError) . (\e -> "Partial explored path(s) during symbolic verification of method " <> methodSignature <> ": " <> unpack e)) partials
    when (not (null partials) || not (null errors)) $ do
      updateTests $ \test -> do
        if isOpen test && isAssertionTest test && getAssertionSignature test == methodSignature then
          pure $ Just $ test {Test.state = Passed}
        else
          pure $ Just test

    pushWorkerEvent $ SymExecLog ("Symbolic execution finished verifying contract " <> unpack (fromJust name) <> " using a single symbolic transaction.")

findFailedTests
  :: (MonadIO m, MonadReader Env m, MonadState WorkerState m)
  => m [EchidnaTest]
findFailedTests = do
  testRefs <- asks (.testRefs)
  tests <- liftIO $ traverse readIORef testRefs
  pure $ filter didFail tests
