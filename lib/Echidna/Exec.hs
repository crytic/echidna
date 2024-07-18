{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Echidna.Exec where

import Optics.Core
import Optics.State.Operators

import Control.Monad (when, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Strict (MonadState(get, put), execState, runStateT, MonadIO(liftIO), gets, modify', execStateT)
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.ST (ST, stToIO, RealWorld)
import Data.Bits
import Data.ByteString qualified as BS
import Data.IORef (readIORef, atomicWriteIORef, newIORef, writeIORef, modifyIORef')
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed.Mutable qualified as VMut
import System.Process (readProcessWithExitCode)

import EVM (bytecode, replaceCodeOfSelf, loadContract, exec1, vmOpIx, clearTStorages)
import EVM.ABI
import EVM.Dapp (DappInfo)
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Fetch qualified
import EVM.Format (hexText, showTraceTree)
import EVM.Types hiding (Env, Gas)

import Echidna.Events (emptyEvents)
import Echidna.Onchain (safeFetchContractFrom, safeFetchSlotFrom)
import Echidna.SourceMapping (lookupUsingCodehashOrInsert)
import Echidna.Symbolic (forceBuf)
import Echidna.Transaction
import Echidna.Types (ExecException(..), Gas, fromEVM, emptyAccount)
import Echidna.Types.Config (Env(..), EConfig(..), UIConf(..), OperationMode(..), OutputFormat(Text))
import Echidna.Types.Coverage (CoverageInfo)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (TxCall(..), Tx, TxResult(..), call, dst, initialTimestamp, initialBlockNumber, getResult)
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
  _                    -> UnknownE

-- | Extracts the 'Query' if there is one.
getQuery :: VMResult Concrete s -> Maybe (Query Concrete s)
getQuery (HandleEffect (Query q)) = Just q
getQuery _ = Nothing

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult Concrete s
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult Concrete s
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | Given an execution error, throw the appropriate exception.
-- Also optionally takes a DappInfo and VM, which are used to show the stack trace.
vmExcept :: MonadThrow m => Maybe (DappInfo, VM Concrete RealWorld) -> EvmError -> m ()
vmExcept traceInfo e =
  let trace = uncurry showTraceTree <$> traceInfo
  in throwM $
    case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e trace}

execTxWith
  :: (MonadIO m, MonadState (VM Concrete RealWorld) m, MonadReader Env m, MonadThrow m)
  => m (VMResult Concrete RealWorld)
  -> Tx
  -> m (VMResult Concrete RealWorld, Gas)
execTxWith executeTx tx = do
  vm <- get
  if hasSelfdestructed vm tx.dst then
    pure (VMFailure (Revert (ConcreteBuf "")), 0)
  else do
    #traces .= emptyEvents
    vmBeforeTx <- get
    setupTx tx
    case tx.call of
      NoCall -> pure (VMSuccess (ConcreteBuf ""), 0)
      _ -> do
        gasLeftBeforeTx <- gets (.state.gas)
        vmResult <- runFully
        gasLeftAfterTx <- gets (.state.gas)
        handleErrorsAndConstruction vmResult vmBeforeTx
        fromEVM clearTStorages
        pure (vmResult, gasLeftBeforeTx - gasLeftAfterTx)
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
        cacheRef <- asks (.fetchContractCache)
        cache <- liftIO $ readIORef cacheRef
        case Map.lookup addr cache of
          Just (Just contract) -> fromEVM (continuation contract)
          Just Nothing -> do
            v <- get
            v' <- liftIO $ stToIO $ execStateT (continuation emptyAccount) v
            put v'
          Nothing -> do
            logMsg $ "INFO: Performing RPC: " <> show q
            case config.rpcUrl of
              Just rpcUrl -> do
                ret <- liftIO $ safeFetchContractFrom rpcBlock rpcUrl addr
                case ret of
                  -- TODO: fix hevm to not return an empty contract in case of an error
                  Just contract | contract.code /= RuntimeCode (ConcreteRuntimeCode "") -> do
                    fromEVM (continuation contract)
                    liftIO $ atomicWriteIORef cacheRef $ Map.insert addr (Just contract) cache
                  _ -> do
                    -- TODO: better error reporting in HEVM, when intermittent
                    -- network error then retry
                    liftIO $ atomicWriteIORef cacheRef $ Map.insert addr Nothing cache
                    logMsg $ "ERROR: Failed to fetch contract: " <> show q
                    -- TODO: How should we fail here? It could be a network error,
                    -- RPC server returning junk etc.
                    fromEVM (continuation emptyAccount)
              Nothing -> do
                liftIO $ atomicWriteIORef cacheRef $ Map.insert addr Nothing cache
                logMsg $ "ERROR: Requested RPC but it is not configured: " <> show q
                -- TODO: How should we fail here? RPC is not configured but VM
                -- wants to fetch
                fromEVM (continuation emptyAccount)
        runFully -- resume execution

      -- A previously unknown slot is required
      Just q@(PleaseFetchSlot addr slot continuation) -> do
        cacheRef <- asks (.fetchSlotCache)
        cache <- liftIO $ readIORef cacheRef
        case Map.lookup addr cache >>= Map.lookup slot of
          Just (Just value) -> fromEVM (continuation value)
          Just Nothing -> fromEVM (continuation 0)
          Nothing -> do
            logMsg $ "INFO: Performing RPC: " <> show q
            case config.rpcUrl of
              Just rpcUrl -> do
                ret <- liftIO $ safeFetchSlotFrom rpcBlock rpcUrl addr slot
                case ret of
                  Just value -> do
                    fromEVM (continuation value)
                    liftIO $ atomicWriteIORef cacheRef $
                      Map.insertWith Map.union addr (Map.singleton slot (Just value)) cache
                  Nothing -> do
                    -- TODO: How should we fail here? It could be a network error,
                    -- RPC server returning junk etc.
                    logMsg $ "ERROR: Failed to fetch slot: " <> show q
                    liftIO $ atomicWriteIORef cacheRef $
                      Map.insertWith Map.union addr (Map.singleton slot Nothing) cache
                    fromEVM (continuation 0)
              Nothing -> do
                logMsg $ "ERROR: Requested RPC but it is not configured: " <> show q
                -- Use the zero slot
                fromEVM (continuation 0)
        runFully -- resume execution

      -- Execute a FFI call
      Just (PleaseDoFFI (cmd : args) continuation) -> do
        (_, stdout, _) <- liftIO $ readProcessWithExitCode cmd args ""
        let encodedResponse = encodeAbiValue $
              AbiTuple (V.fromList [AbiBytesDynamic . hexText . T.pack $ stdout])
        fromEVM (continuation encodedResponse)
        runFully

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
      -- If a transaction reverts reset VM to state before the transaction.
      put vmBeforeTx
      -- Undo reset of some of the VM state.
      -- Otherwise we'd loose all information about the reverted transaction like
      -- contract address, calldata, result and traces.
      #result ?= vmResult
      #state % #calldata .= calldataBeforeVMReset
      #state % #callvalue .= callvalueBeforeVMReset
      #traces .= tracesBeforeVMReset
      #state % #codeContract .= codeContractBeforeVMReset
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
  => VM Concrete RealWorld
  -> Tx
  -> m ((VMResult Concrete RealWorld, Gas), VM Concrete RealWorld)
execTx vm tx = runStateT (execTxWith (fromEVM exec) tx) vm

-- | A type alias for the context we carry while executing instructions
type CoverageContext = (Bool, Maybe (VMut.IOVector CoverageInfo, Int))

-- | Execute a transaction, logging coverage at every step.
execTxWithCov
  :: (MonadIO m, MonadState (VM Concrete RealWorld) m, MonadReader Env m, MonadThrow m)
  => Tx
  -> m ((VMResult Concrete RealWorld, Gas), Bool)
execTxWithCov tx = do
  env <- ask

  covContextRef <- liftIO $ newIORef (False, Nothing)

  r <- execTxWith (execCov env covContextRef) tx

  (grew, lastLoc) <- liftIO $ readIORef covContextRef

  -- Update the last valid location with the transaction result
  grew' <- liftIO $ case lastLoc of
    Just (vec, pc) -> do
      let txResultBit = fromEnum $ getResult $ fst r
      VMut.read vec pc >>= \case
        (opIx, depths, txResults) | not (txResults `testBit` txResultBit) -> do
          VMut.write vec pc (opIx, depths, txResults `setBit` txResultBit)
          pure True -- we count this as new coverage
        _ -> pure False
    _ -> pure False

  pure (r, grew || grew')
  where
    -- the same as EVM.exec but collects coverage, will stop on a query
    execCov env covContextRef = do
      vm <- get
      (r, vm') <- liftIO $ loop vm
      put vm'
      pure r
      where
      -- | Repeatedly exec a step and add coverage until we have an end result
      loop :: VM Concrete RealWorld -> IO (VMResult Concrete RealWorld, VM Concrete RealWorld)
      loop !vm = case vm.result of
        Nothing -> do
          addCoverage vm
          stepVM vm >>= loop
        Just r -> pure (r, vm)

      -- | Execute one instruction on the EVM
      stepVM :: VM Concrete RealWorld -> IO (VM Concrete RealWorld)
      stepVM = stToIO . execStateT exec1

      -- | Add current location to the CoverageMap
      addCoverage :: VM Concrete RealWorld -> IO ()
      addCoverage !vm = do
        let (pc, opIx, depth) = currentCovLoc vm
            contract = currentContract vm

        maybeCovVec <- lookupUsingCodehashOrInsert env.codehashMap contract env.dapp env.coverageRef $ do
          let size = BS.length . forceBuf . fromJust . view bytecode $ contract
          if size == 0 then pure Nothing else do
            -- IO for making a new vec
            vec <- VMut.new size
            -- We use -1 for opIx to indicate that the location was not covered
            forM_ [0..size-1] $ \i -> VMut.write vec i (-1, 0, 0)
            pure $ Just vec

        case maybeCovVec of
          Nothing -> pure ()
          Just vec -> do
            -- TODO: no-op when pc is out-of-bounds. This shouldn't happen but
            -- we observed this in some real-world scenarios. This is likely a
            -- bug in another place, investigate.
            -- ... this should be fixed now, since we use `codeContract` instead
            -- of `contract` for everything; it may be safe to remove this check.
            when (pc < VMut.length vec) $
              VMut.read vec pc >>= \case
                (_, depths, results) | depth < 64 && not (depths `testBit` depth) -> do
                  VMut.write vec pc (opIx, depths `setBit` depth, results `setBit` fromEnum Stop)
                  writeIORef covContextRef (True, Just (vec, pc))
                _ ->
                  modifyIORef' covContextRef $ \(new, _) -> (new, Just (vec, pc))

      -- | Get the VM's current execution location
      currentCovLoc vm = (vm.state.pc, fromMaybe 0 $ vmOpIx vm, length vm.frames)

      -- | Get the current contract being executed
      currentContract vm = fromMaybe (error "no contract information on coverage") $
        vm ^? #env % #contracts % at vm.state.codeContract % _Just

initialVM :: Bool -> ST s (VM Concrete s)
initialVM ffi = do
  vm <- vmForEthrunCreation mempty
  pure $ vm & #block % #timestamp .~ Lit initialTimestamp
            & #block % #number .~ initialBlockNumber
            & #env % #contracts .~ mempty -- fixes weird nonce issues
            & #config % #allowFFI .~ ffi
