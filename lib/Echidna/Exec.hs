{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Echidna.Exec
  ( execTx
  , execTxWithCov
  , initialVM
  , pattern Reversion
  ) where

import Control.Monad (void, when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.ST (ST, stToIO, RealWorld)
import Control.Monad.State.Strict (MonadState(get, put), execState, runStateT, MonadIO(liftIO), gets, modify', execStateT)
import Data.Bits
import Data.ByteString qualified as BS
import Data.IORef (readIORef, newIORef, writeIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Primitive.PrimArray (readPrimArray)
import Data.Primitive.PrimVar (fetchAddInt)
import Data.Text qualified as T
import Data.Vector qualified as V
import Optics.Core
import Optics.State.Operators
import System.Environment (lookupEnv, getEnvironment)
import System.Process qualified as P

import EVM (bytecode, replaceCodeOfSelf, loadContract, exec1, clearTStorages, currentContract)
import EVM.ABI
import EVM.Dapp (DappInfo(..))
import EVM.Effects (defaultConfig)
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Fetch qualified
import EVM.Format (hexText, showTraceTree)
import EVM.Solidity (SolcContract(..))
import EVM.Types hiding (Env, Gas)

import Echidna.Events (emptyEvents)
import Echidna.Onchain (safeFetchContractFrom, safeFetchSlotFrom)
import Echidna.SourceMapping (lookupUsingCodehashOrInsert)
import Echidna.SymExec.Symbolic (forceBuf)
import Echidna.Transaction
import Echidna.Types (ExecException(..), fromEVM, emptyAccount)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..), UIConf(..), OperationMode(..), OutputFormat(Text))
import Echidna.Types.Coverage (CovEntry(..), CovSlot, newCovEntry)
import Echidna.Types.Coverage.Atomic (fetchOrPrimArray)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (TxCall(..), Tx(call, dst, delay), TxResult(..), initialTimestamp, initialBlockNumber, getResult)
import Echidna.Utility (getTimestamp, timePrefix)

-- | Broad categories of execution failures: reversions, illegal operations, and ???.
data ErrorClass = RevertE | IllegalE | UnknownE

-- | Given an execution error, classify it. Mostly useful for nice @pattern@s ('Reversion', 'Illegal').
classifyError :: EvmError -> ErrorClass
classifyError = \case
  OutOfGas _ _         -> RevertE
  Revert _             -> RevertE
  UnrecognizedOpcode _ -> RevertE
  StackLimitExceeded   -> RevertE
  StackUnderrun        -> IllegalE
  BadJumpDestination   -> IllegalE
  IllegalOverflow      -> RevertE
  AssumeCheatFailed    -> RevertE
  _                    -> UnknownE

-- | Extracts the 'Query' if there is one.
getQuery :: VMResult Concrete -> Maybe (Query Concrete)
getQuery (HandleEffect (Query q)) = Just q
getQuery _ = Nothing

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult Concrete
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult Concrete
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | Given an execution error, throw the appropriate exception.
-- Also optionally takes a DappInfo and VM, which are used to show the stack trace.
vmExcept :: MonadThrow m => Maybe (DappInfo, VM Concrete) -> EvmError -> m ()
vmExcept traceInfo e =
  let trace = uncurry showTraceTree <$> traceInfo
  in throwM $
    case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e trace}

execTxWith
  :: (MonadIO m, MonadState (VM Concrete) m, MonadReader Env m, MonadThrow m)
  => m (VMResult Concrete)
  -> Tx
  -> m (VMResult Concrete)
execTxWith executeTx tx = do
  vm <- get
  if hasSelfdestructed vm tx.dst then
    pure $ VMFailure (Revert (ConcreteBuf ""))
  else do
    config <- asks (.cfg)
    when (not config.allEvents) $ #traces .= emptyEvents
    vmBeforeTx <- get
    setupTx tx
    case tx.call of
      NoCall -> pure $ VMSuccess (ConcreteBuf "")
      _ -> do
        vmResult <- runFully
        handleErrorsAndConstruction vmResult vmBeforeTx
        fromEVM clearTStorages
        pure vmResult
  where
  runFully = do
    config <- asks (.cfg)
    -- TODO: Is the latest block a good default? It makes fuzzing hard to reproduce. Rethink this.
    let rpcBlock = maybe EVM.Fetch.Latest (EVM.Fetch.BlockNumber . fromIntegral) config.rpcBlock

    vmResult <- executeTx
    -- For queries, we halt execution because the VM needs some additional
    -- information from the outside. We provide this information and resume
    -- the execution by recursively calling `runFully`.
    case getQuery vmResult of
      -- A previously unknown contract is required
      Just q@(PleaseFetchContract addr _ continuation) -> do
        --logMsg $ "INFO: Performing RPC: " <> show q
        case config.rpcUrl of
          Just rpcUrl -> do
            session <- asks (.fetchSession)
            ret <- liftIO $ safeFetchContractFrom session rpcBlock rpcUrl addr
            case ret of
              EVM.Fetch.FetchSuccess contract _ -> do
                fromEVM (continuation contract)
              EVM.Fetch.FetchFailure _ -> do
                fromEVM (continuation emptyAccount)
              EVM.Fetch.FetchError e -> do
                error $ "ERROR: Failed to fetch contract: " <> show q <> " " <> T.unpack e
          Nothing -> do
            --logMsg $ "ERROR: Requested RPC but it is not configured: " <> show q
            -- TODO: How should we fail here? RPC is not configured but VM
            -- wants to fetch
            fromEVM (continuation emptyAccount)
        runFully -- resume execution

      -- A previously unknown slot is required
      Just q@(PleaseFetchSlot addr slot continuation) -> do
        case config.rpcUrl of
          Just rpcUrl -> do
            session <- asks (.fetchSession)
            ret <- liftIO $ safeFetchSlotFrom session rpcBlock rpcUrl addr slot
            case ret of
              EVM.Fetch.FetchSuccess value status -> do
                -- Log only in text mode, ignoring quiet flag as this is important info
                when (status == EVM.Fetch.Fresh) $ logMsg $ "Fetched new slot: " <> show q
                fromEVM (continuation value)
              EVM.Fetch.FetchFailure _ -> do
                fromEVM (continuation 0)
              EVM.Fetch.FetchError e -> do
                error $ "ERROR: Failed to fetch slot: " <> show q <> " " <> T.unpack e
          Nothing -> do
            --logMsg $ "ERROR: Requested RPC but it is not configured: " <> show q
            -- Use the zero slot
            fromEVM (continuation 0)
        runFully -- resume execution

      -- Execute a FFI call
      Just (PleaseDoFFI (cmd : args) envs continuation) -> do
        existingEnv <- liftIO getEnvironment
        let mergedEnv = Map.toList $ Map.union envs $ Map.fromList existingEnv
        let process = (P.proc cmd args) { P.env = Just mergedEnv }
        (_, stdout, _) <- liftIO $ P.readCreateProcessWithExitCode process ""
        let encodedResponse = encodeAbiValue $
              AbiTuple (V.fromList [AbiBytesDynamic . hexText . T.strip . T.pack $ stdout])
        fromEVM (continuation encodedResponse)
        runFully

      Just (PleaseReadEnv var continuation) -> do
        value <- liftIO $ lookupEnv var
        fromEVM (continuation $ fromMaybe "" value)
        runFully -- resume execution

      -- No queries to answer, the tx is fully executed and the result is final
      _ -> pure vmResult

  -- | Handles reverts, failures and contract creations that might be the result
  -- (`vmResult`) of executing transaction `tx`.
  handleErrorsAndConstruction vmResult vmBeforeTx = case (vmResult, tx.call) of
    (Reversion, _) -> do
      tracesBeforeVMReset <- gets (.traces)
      codeContractBeforeVMReset <- gets (.state.codeContract)
      calldataBeforeVMReset <- gets (.state.calldata)
      callvalueBeforeVMReset <- gets (.state.callvalue)
      burnedGas <- gets (.burned)
      -- If a transaction reverts reset VM to state before the transaction.
      put vmBeforeTx
      -- Re-apply the time/block advancement from the transaction's delay.
      -- Time doesn't go backwards on revert, only EVM state does.
      #block %= \b -> advanceBlock b tx.delay
      -- Undo reset of some of the VM state.
      -- Otherwise we'd lose all information about the reverted transaction like
      -- contract address, calldata, result and traces.
      #result ?= vmResult
      #state % #calldata .= calldataBeforeVMReset
      #state % #callvalue .= callvalueBeforeVMReset
      #traces .= tracesBeforeVMReset
      #state % #codeContract .= codeContractBeforeVMReset
      #burned .= burnedGas
    (VMFailure x, _) -> do
      dapp <- asks (.dapp)
      vm <- get
      vmExcept (Just (dapp, vm)) x
    (VMSuccess (ConcreteBuf bytecode'), SolCreate _) -> do
      -- Handle contract creation.
      #env % #contracts % at (LitAddr tx.dst) % _Just % #code .= InitCode mempty mempty
      fromEVM $ replaceCodeOfSelf (RuntimeCode (ConcreteRuntimeCode bytecode'))
      modify' $ execState $ loadContract (LitAddr tx.dst)
    _ -> pure ()

logMsg :: (MonadIO m, MonadReader Env m) => String -> m ()
logMsg msg = do
  cfg <- asks (.cfg)
  operationMode <- asks (.cfg.uiConf.operationMode)
  when (operationMode == NonInteractive Text && not cfg.solConf.quiet) $ liftIO $ do
    time <- timePrefix <$> getTimestamp
    putStrLn $ time <> msg

-- | Execute a transaction "as normal".
execTx
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => VM Concrete
  -> Tx
  -> m (VMResult Concrete, VM Concrete)
execTx vm tx = runStateT (execTxWith (fromEVM (exec defaultConfig)) tx) vm

-- | A type alias for the context we carry while executing instructions:
-- whether new coverage was found, and the last covered location.
type CoverageContext = (Bool, Maybe (CovEntry, Int))

-- | The coverage entry of the code being executed, looked up once per
-- contract switch rather than once per instruction. The executing code only
-- changes at call boundaries, so nearly every step reuses it.
data CoverageCache = CoverageCache
  { code     :: ContractCode
    -- ^ the key: the code executing at the previous step. Coverage is keyed
    -- by codehash, so equal code means the same entry whatever the address.
    -- The 'Eq' short-circuits to a pointer comparison of the underlying
    -- 'BS.ByteString' when it is the same object, which the loop maintains
    -- by storing the current step's code on every hit.
  , covEntry :: Maybe CovEntry
    -- ^ its coverage entry, if it has any code at all
  }

-- | Execute a transaction, logging coverage at every step.
execTxWithCov
  :: (MonadIO m, MonadState (VM Concrete) m, MonadReader Env m, MonadThrow m)
  => CovSlot -- ^ private coverage slot of the calling agent
  -> Tx
  -> m (VMResult Concrete, Bool)
execTxWithCov _slot tx = do
  env <- ask

  covContextRef <- liftIO $ newIORef (False, Nothing)

  r <- execTxWith (execCov env covContextRef) tx

  (grew, lastLoc) <- liftIO $ readIORef covContextRef

  -- Record the transaction result at the last executed location. Seeing a
  -- new result there counts as new coverage; the old value returned by the
  -- atomic OR says whether this worker was the first to see it.
  grew' <- liftIO $ case lastLoc of
    Just (entry, pc) -> do
      let txResultBit = fromEnum $ getResult r
      old <- fetchOrPrimArray entry.bits (2 * pc + 1) (bit txResultBit)
      pure $ not (old `testBit` txResultBit)
    _ -> pure False

  pure (r, grew || grew')
  where
    -- the same as EVM.exec but collects coverage, will stop on a query
    execCov env covContextRef = do
      vm <- get
      -- The context survives across calls: execution resumes here after
      -- answering a query, and the last location must span the whole tx.
      ctx <- liftIO $ readIORef covContextRef
      (r, vm', ctx') <- liftIO $ loop Nothing ctx vm
      liftIO $ writeIORef covContextRef ctx'
      put vm'
      pure r
      where
      -- | Repeatedly exec a step and add coverage until we have an end result
      loop
        :: Maybe CoverageCache -> CoverageContext -> VM Concrete
        -> IO (VMResult Concrete, VM Concrete, CoverageContext)
      loop cache !ctx !vm = case vm.result of
        Nothing -> do
          (cache', ctx') <- addCoverage env cache ctx vm
          vm' <- stepVM vm
          loop (Just cache') ctx' vm'
        Just r -> pure (r, vm, ctx)

      -- | Execute one instruction on the EVM
      stepVM :: VM Concrete -> IO (VM Concrete)
      stepVM = stToIO . execStateT (exec1 defaultConfig)

-- | Add current location to the CoverageMap
addCoverage
  :: Env -> Maybe CoverageCache -> CoverageContext -> VM Concrete
  -> IO (CoverageCache, CoverageContext)
addCoverage env cache ctx@(grew, _) !vm = do
  cache'@CoverageCache{covEntry} <- case cache of
    -- Hit: keep this step's code object so the next comparison is a
    -- pointer check even if we just switched to a contract whose code is
    -- equal but not the same object (a clone).
    Just c | c.code == vm.state.code ->
      pure CoverageCache { code = vm.state.code, covEntry = c.covEntry }
    _ -> lookupCoverage env vm

  ctx' <- case covEntry of
    Nothing -> pure ctx
    Just entry -> do
      let pc = vm.state.pc
          depth = length vm.frames
      -- The loop observes `pc == len` right before hevm's implicit STOP, and a
      -- truncated PUSH can leave pc past the end; neither is a location.
      if pc >= entry.len then pure ctx else do
        depths <- readPrimArray entry.bits (2 * pc)
        if depth < 64 && not (depths `testBit` depth)
          then do
            -- Rare path: publish the depth bit, and the Stop bit that marks
            -- the pc as executed for the report. The old value tells whether
            -- this worker set the depth bit first.
            old <- fetchOrPrimArray entry.bits (2 * pc) (bit depth)
            _ <- fetchOrPrimArray entry.bits (2 * pc + 1) (bit (fromEnum Stop))
            -- A pc counts as a new unique instruction exactly once.
            when (old == 0) $ void $ fetchAddInt env.coveragePoints 1
            pure (grew || not (old `testBit` depth), Just (entry, pc))
          else pure (grew, Just (entry, pc))

  pure (cache', ctx')

-- | Find (or create) the coverage entry of the contract being executed
lookupCoverage :: Env -> VM Concrete -> IO CoverageCache
lookupCoverage env vm = do
  let contract = fromMaybe (error "no contract information on coverage") $ currentContract vm
      covRef = case contract.code of
        InitCode _ _ -> env.coverageRefInit
        _ -> env.coverageRefRuntime

  maybeEntry <- lookupUsingCodehashOrInsert env.codehashMap contract env.dapp covRef $ \key -> do
    let
      size = case contract.code of
        InitCode b _ -> BS.length b
        _ -> BS.length . forceBuf . fromJust . view bytecode $ contract
      -- The key is a compile-time hash, so it finds its contract in the dapp
      -- unless the code is unknown to the build, in which case it owns itself.
      owner = maybe key ((.runtimeCodehash) . snd) $ Map.lookup key env.dapp.solcByHash
    if size == 0 then pure Nothing else Just <$> newCovEntry owner contract.opIxMap size

  pure CoverageCache { code = vm.state.code, covEntry = maybeEntry }

initialVM :: EConfig -> ST RealWorld (VM Concrete)
initialVM cfg = do
  vm <- vmForEthrunCreation mempty
  let !allowFFI = cfg.solConf.allowFFI
      !recordKeccakPreImgs = cfg.campaignConf.symExec
  pure $ vm & #block % #timestamp .~ Lit initialTimestamp
            & #block % #number .~ Lit initialBlockNumber
            & #env % #contracts .~ mempty -- fixes weird nonce issues
            & #config % #allowFFI .~ allowFFI
            & #config % #recordKeccakPreImgs .~ recordKeccakPreImgs
