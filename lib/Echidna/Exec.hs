{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Echidna.Exec
  ( execTx
  , execTxWithCov
  , initialVM
  , pattern Reversion
  ) where

import Control.Monad (when, forM_)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Reader (MonadReader, ask, asks)
import Control.Monad.ST (ST, stToIO, RealWorld)
import Control.Monad.State.Strict (MonadState(get, put), execState, runStateT, MonadIO(liftIO), gets, modify', execStateT)
import Data.Bits
import Data.ByteString qualified as BS
import Data.IORef (readIORef, newIORef, writeIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as VMut
import Optics.Core
import Optics.State.Operators
import System.Environment (lookupEnv, getEnvironment)
import System.Process qualified as P

import EVM (bytecode, replaceCodeOfSelf, loadContract, exec1, clearTStorages, currentContract)
import EVM.ABI
import EVM.Concrete.Blocks (Program, Scratch, newScratch, prepareProgram, runConcreteBlocks)
import EVM.Dapp (DappInfo)
import EVM.Effects (defaultConfig)
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Fetch qualified
import EVM.Format (hexText, showTraceTree)
import EVM.Types hiding (Env, Gas)

import Echidna.Events (emptyEvents)
import Echidna.Onchain (safeFetchContractFrom, safeFetchSlotFrom)
import Echidna.SourceMapping (lookupUsingCodehashOrInsert)
import Echidna.SymExec.Symbolic (forceBuf)
import Echidna.Transaction
import Echidna.Types (ExecException(..), fromEVM, emptyAccount)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..), UIConf(..), OperationMode(..), OutputFormat(Text))
import Echidna.Types.Coverage (CoverageInfo, StackDepths)
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
    selectors <- asks (.functionSelectors)
    setupTx selectors tx
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

-- | A type alias for the context we carry while executing instructions
type CoverageContext = (Bool, Maybe (VMut.IOVector CoverageInfo, Int))

-- | Coverage data reused while the executing code is unchanged.
data CoverageCache = CoverageCache
  { code    :: ContractCode
    -- ^ Equal code shares coverage across contract addresses.
  , stackProgram :: Maybe Program
  , opIxMap :: VS.Vector Int
  , covVec  :: Maybe (VMut.IOVector CoverageInfo)
  }

-- | Execute a transaction, logging coverage at every step.
execTxWithCov
  :: (MonadIO m, MonadState (VM Concrete) m, MonadReader Env m, MonadThrow m)
  => Tx
  -> m (VMResult Concrete, Bool)
execTxWithCov tx = do
  env <- ask

  covContextRef <- liftIO $ newIORef (False, Nothing)

  r <- execTxWith (execCov env covContextRef) tx

  (grew, lastLoc) <- liftIO $ readIORef covContextRef

  -- Update the last valid location with the transaction result
  grew' <- liftIO $ case lastLoc of
    Just (vec, pc) -> do
      let txResultBit = fromEnum $ getResult r
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
      -- Preserve coverage state across VM queries.
      ctx <- liftIO $ readIORef covContextRef
      (r, vm', ctx') <- liftIO $ do
        buffer <- newScratch
        loop buffer Nothing ctx vm
      liftIO $ writeIORef covContextRef ctx'
      put vm'
      pure r
      where
      -- | Repeatedly exec a step and add coverage until we have an end result
      loop :: Scratch -> Maybe CoverageCache -> CoverageContext -> VM Concrete
           -> IO (VMResult Concrete, VM Concrete, CoverageContext)
      loop buffer cache !ctx !vm = case vm.result of
        Nothing -> do
          cache' <- case cache of
            Just c | c.code == vm.state.code -> pure (c { code = vm.state.code } :: CoverageCache)
            _ -> lookupCoverage env vm
          let depth = length vm.frames
              mask = if depth < 64 then bit depth else 0
              visit = visitCoverage cache' mask
          (before, ctx') <- runConcreteBlocks buffer cache'.stackProgram visit (visitBlock cache' mask) vm ctx
          ctx'' <- visit before.state.pc ctx'
          vm' <- stepVM before
          loop buffer (Just cache') ctx'' vm'
        Just r -> pure (r, vm, ctx)

      -- | Execute one instruction on the EVM
      stepVM :: VM Concrete -> IO (VM Concrete)
      stepVM = stToIO . execStateT (exec1 defaultConfig)

-- | Both callers have checked the vector bounds. Return the accumulated
-- growth flag so a block need only construct its final coverage context.
recordPC :: VS.Vector Int -> VMut.IOVector CoverageInfo -> StackDepths -> Int -> Bool -> IO Bool
recordPC opIxMap vec mask pc grew = do
  let (opIxs, depthBits, resultBits) = VMut.unzip3 vec
  depths <- VMut.unsafeRead depthBits pc
  if mask /= 0 && depths .&. mask == 0
    then do
      results <- VMut.unsafeRead resultBits pc
      VMut.unsafeWrite opIxs pc (fromMaybe 0 (opIxMap VS.!? pc))
      VMut.unsafeWrite depthBits pc (depths .|. mask)
      VMut.unsafeWrite resultBits pc (results `setBit` fromEnum Stop)
      pure True
    else pure grew
{-# INLINE recordPC #-}

visitCoverage :: CoverageCache -> StackDepths -> Int -> CoverageContext -> IO CoverageContext
visitCoverage CoverageCache{opIxMap, covVec = Just vec} depth pc (grew, previous)
  | pc >= 0 && pc < VMut.length vec = do
      changed <- recordPC opIxMap vec depth pc grew
      pure (changed, Just (vec, pc))
  | otherwise = pure (grew, previous)
visitCoverage _ _ _ ctx = pure ctx

-- The program supplies only non-negative instruction offsets. Preserve the
-- previous location if none of these PCs belongs to this coverage vector.
visitBlock :: CoverageCache -> StackDepths -> U.Vector Int -> CoverageContext -> IO CoverageContext
visitBlock CoverageCache{opIxMap, covVec = Just vec} depth pcs ctx@(grew, _) = loop 0 grew (-1)
  where
    loop !i !changed !lastPC
      | i == U.length pcs = pure $ if lastPC < 0 then ctx else (changed, Just (vec, lastPC))
      | otherwise = do
          let pc = U.unsafeIndex pcs i
          if pc >= VMut.length vec then loop (i+1) changed lastPC else do
            changed' <- recordPC opIxMap vec depth pc changed
            loop (i+1) changed' pc
visitBlock _ _ _ ctx = pure ctx

lookupCoverage :: Env -> VM Concrete -> IO CoverageCache
lookupCoverage env vm = do
  let contract = fromMaybe (error "no contract information on coverage") $ currentContract vm
      covRef = case contract.code of
        InitCode _ _ -> env.coverageRefInit
        _ -> env.coverageRefRuntime

  maybeCovVec <- lookupUsingCodehashOrInsert env.codehashMap contract env.dapp covRef $ do
    let
      size = case contract.code of
        InitCode b _ -> BS.length b
        _ -> BS.length . forceBuf . fromJust . view bytecode $ contract
    if size == 0 then pure Nothing else do
      -- IO for making a new vec
      vec <- VMut.new size
      -- We use -1 for opIx to indicate that the location was not covered
      forM_ [0..size-1] $ \i -> VMut.write vec i (-1, 0, 0)
      pure $ Just vec

  stackProgram <- prepareProgram env.stackPrograms vm.state.code
  pure CoverageCache { code = vm.state.code
                     , stackProgram
                     , opIxMap = contract.opIxMap
                     , covVec = maybeCovVec
                     }

initialVM :: EConfig -> ST RealWorld (VM Concrete)
initialVM cfg = do
  vm <- vmForEthrunCreation mempty
  -- Symbolic execution needs hashes from deployment and earlier concrete
  -- transactions, including replays of transactions found by the solver.
  let !allowFFI = cfg.solConf.allowFFI
      !recordKeccakPreImgs = cfg.campaignConf.symExec
  pure $ vm & #block % #timestamp .~ Lit initialTimestamp
            & #block % #number .~ Lit initialBlockNumber
            & #env % #contracts .~ mempty -- fixes weird nonce issues
            & #config % #allowFFI .~ allowFFI
            & #config % #recordKeccakPreImgs .~ recordKeccakPreImgs
