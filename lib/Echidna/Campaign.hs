{-# LANGUAGE GADTs #-}

module Echidna.Campaign where

import Optics.Core

import Control.DeepSeq (force)
import Control.Monad (foldM, replicateM, when, unless, void)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom, RandT, evalRandT)
import Control.Monad.Reader (MonadReader, asks, liftIO, ask)
import Control.Monad.State.Strict
  (MonadState(..), StateT(..), evalStateT, execStateT, gets, MonadIO, modify')
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Random.Strict (liftCatch)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (readIORef, writeIORef)
import Data.Map qualified as Map
import Data.Map (Map, (\\))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import System.Random (mkStdGen)

import EVM hiding (Env, Frame(state), VM(state))
import EVM (VM)
import EVM.ABI (getAbi, AbiType(AbiAddressType), AbiValue(AbiAddress))
import EVM.Types (Addr, Expr(ConcreteBuf))

import Echidna.ABI
import Echidna.Exec
import Echidna.Events (extractEvents)
import Echidna.Mutator.Corpus
import Echidna.Shrink (shrinkTest)
import Echidna.Test
import Echidna.Transaction
import Echidna.Types (Gas)
import Echidna.Types.Buffer (forceBuf)
import Echidna.Types.Corpus (Corpus)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Signature (makeBytecodeCache, FunctionName)
import Echidna.Types.Test
import Echidna.Types.Tx (TxCall(..), Tx(..), call)
import Echidna.Types.World (World)

instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM
instance MonadCatch m => MonadCatch (RandT g m) where
  catch = liftCatch catch

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: MonadReader Env m => GenericCampaign a -> m Bool
isDone c | null c.tests = do
  conf <- asks (.cfg.campaignConf)
  pure $ c.ncallseqs * conf.seqLen >= conf.testLimit
isDone c = do
  conf <- asks (.cfg.campaignConf)
  let
    result = \case
      Open i   -> if i >= conf.testLimit then Just True else Nothing
      Passed   -> Just True
      Large i  -> if i >= conf.shrinkLimit then Just False else Nothing
      Solved   -> Just False
      Failed _ -> Just False
    testResults = result . (.state) <$> c.tests
    done = if conf.stopOnFail then Just False `elem` testResults
                              else all isJust testResults
  pure done

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccessful :: Campaign -> Bool
isSuccessful Campaign{tests} =
  all (\case { Passed -> True; Open _ -> True; _ -> False; }) ((.state) <$> tests)

-- | Run all the transaction sequences from the corpus and accumulate campaign
-- state. Can be used to minimize corpus as the final campaign state will
-- contain minized corpus without sequences that didn't increase the coverage.
replayCorpus
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m, MonadState Campaign m)
  => VM     -- ^ VM to start replaying from
  -> [[Tx]] -- ^ corpus to replay
  -> m VM   -- ^ VM after corpus replay
replayCorpus = foldM callseq

-- | Run a fuzzing campaign given an initial universe state, some tests, and an
-- optional dictionary to generate calls with. Return the 'Campaign' state once
-- we can't solve or shrink anything.
runCampaign
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m)
  => StateT Campaign m Bool
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM            -- ^ Initial VM state
  -> World         -- ^ Initial world state
  -> [EchidnaTest] -- ^ Tests to evaluate
  -> GenDict       -- ^ Generation dictionary
  -> [[Tx]]        -- ^ Initial corpus of transactions
  -> m Campaign
runCampaign callback vm world tests dict initialCorpus = do
  conf <- asks (.cfg.campaignConf)
  metaCacheRef <- asks (.metadataCache)
  fetchContractCacheRef <- asks (.fetchContractCache)
  external <- liftIO $ Map.mapMaybe id <$> readIORef fetchContractCacheRef
  liftIO $ writeIORef metaCacheRef (mkMemo (vm.env.contracts <> external))

  let
    covMap = fromMaybe mempty conf.knownCoverage
    effectiveSeed = fromMaybe dict.defSeed conf.seed
    effectiveGenDict = dict { defSeed = effectiveSeed }
    campaign = Campaign { tests = tests
                        , coverage = covMap
                        , gasInfo = mempty
                        , genDict = effectiveGenDict
                        , newCoverage = False
                        , corpus = Set.empty
                        , ncallseqs = 0
                        }

  flip execStateT campaign $ do
    flip evalRandT (mkStdGen effectiveSeed) $ do
      void $ lift callback
      void $ replayCorpus vm initialCorpus
      run

  where
  run = do
    testStates <- gets (fmap (.state) . (.tests))
    CampaignConf{testLimit, stopOnFail, seqLen, shrinkLimit} <- asks (.cfg.campaignConf)
    Campaign{ncallseqs} <- get
    let
      stopEarlier =
        stopOnFail && any (\case Solved -> True; Failed _ -> True; _ -> False)
                          testStates
    if | stopEarlier ->
         void $ lift callback
       | any (\case Open n -> n <= testLimit; _ -> False) testStates ->
         fuzz >> continue
       | any (\case Large n -> n < shrinkLimit; _ -> False) testStates ->
         continue
       | null testStates && (seqLen * ncallseqs) <= testLimit ->
         fuzz >> continue
       | otherwise ->
         void $ lift callback

  fuzz = randseq vm.env.contracts world >>= callseq vm

  continue = do
    runUpdate (shrinkTest vm)
    stop <- lift callback
    unless stop run

  mkMemo = makeBytecodeCache . map (forceBuf . (^. bytecode)) . Map.elems

-- | Generate a new sequences of transactions, either using the corpus or with
-- randomly created transactions
randseq
  :: (MonadRandom m, MonadReader Env m, MonadState Campaign m, MonadIO m)
  => Map Addr Contract
  -> World
  -> m [Tx]
randseq deployedContracts world = do
  env <- ask
  memo <- liftIO $ readIORef env.metadataCache
  campaign <- get

  let
    mutConsts = env.cfg.campaignConf.mutConsts
    txConf = env.cfg.txConf
    seqLen = env.cfg.campaignConf.seqLen

  -- TODO: include reproducer when optimizing
  --let rs = filter (not . null) $ map (.testReproducer) $ ca._tests
  --
  -- Generate new random transactions
  randTxs <- replicateM seqLen (genTx memo world txConf deployedContracts)
  -- Generate a random mutator
  cmut <- if seqLen == 1 then seqMutatorsStateless (fromConsts mutConsts)
                         else seqMutatorsStateful (fromConsts mutConsts)
  -- Fetch the mutator
  let mut = getCorpusMutation cmut
  if null campaign.corpus
    then pure randTxs -- Use the generated random transactions
    else mut seqLen campaign.corpus randTxs -- Apply the mutator

-- | Runs a transaction sequence and checks if any test got falsified or can be
-- minimized. Stores any useful data in the campaign state if coverage increased.
callseq
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m, MonadState Campaign m)
  => VM
  -> [Tx]
  -> m VM
callseq vm txSeq = do
  conf <- asks (.cfg.campaignConf)
  -- First, we figure out whether we need to execute with or without coverage
  -- optimization and gas info, and pick our execution function appropriately
  let
    coverageEnabled = isJust conf.knownCoverage
    execFunc =
      if coverageEnabled
         then execTxOptC
         else \tx -> do
           (v, ca) <- get
           (r, vm') <- runStateT (execTx tx) v
           put (vm', ca)
           pure r
  -- Then, we get the current campaign state
  camp <- get

  -- Run each call sequentially. This gives us the result of each call
  -- and the new state
  (res, (vm', camp')) <- runStateT (evalSeq vm execFunc txSeq) (vm, camp)

  let
    -- compute the addresses not present in the old VM via set difference
    newAddrs = Map.keys $ vm'.env.contracts \\ vm.env.contracts
    -- and construct a set to union to the constants table
    diffs = Map.fromList [(AbiAddressType, Set.fromList $ AbiAddress <$> newAddrs)]
    -- Now we try to parse the return values as solidity constants, and add then to the 'GenDict'
    results = returnValues (map (\(t, (vr, _)) -> (t, vr)) res) camp'.genDict.rTypes
    -- union the return results with the new addresses
    additions = Map.unionWith Set.union diffs results
    -- append to the constants dictionary
    updatedDict = camp'.genDict
      { constants = Map.unionWith Set.union additions camp'.genDict.constants
      , dictValues = Set.union (mkDictValues $ Set.unions $ Map.elems additions)
                               camp'.genDict.dictValues
      }

  -- Update the campaign state
  put camp'
    { genDict = updatedDict
      -- Update the gas estimation
    , gasInfo =
        if conf.estimateGas
           then updateGasInfo res [] camp'.gasInfo
           else camp'.gasInfo
      -- If there is new coverage, add the transaction list to the corpus
    , corpus =
        if camp'.newCoverage
           -- corpus is a bit too lazy, force the evaluation to reduce the memory usage
           then force $ addToCorpus (camp'.ncallseqs + 1) res camp'.corpus
           else camp'.corpus
      -- Reset the new coverage flag
    , newCoverage = False
      -- Keep track of the number of calls to `callseq`
    , ncallseqs = camp'.ncallseqs + 1
    }

  pure vm'
  where
  -- Given a list of transactions and a return typing rule, checks whether we
  -- know the return type for each function called. If yes, tries to parse the
  -- return value as a value of that type. Returns a 'GenDict' style Map.
  returnValues
    :: [(Tx, VMResult)]
    -> (FunctionName -> Maybe AbiType)
    -> Map AbiType (Set AbiValue)
  returnValues txResults returnTypeOf =
    Map.fromList . flip mapMaybe txResults $ \(tx, result) -> do
      case result of
        VMSuccess (ConcreteBuf buf) -> do
          fname <- case tx.call of
            SolCall (fname, _) -> Just fname
            _ -> Nothing
          type' <- returnTypeOf fname
          case runGetOrFail (getAbi type') (LBS.fromStrict buf) of
            -- make sure we don't use cheat codes to form fuzzing call sequences
            Right (_, _, abiValue) | abiValue /= AbiAddress cheatCode ->
              Just (type', Set.singleton abiValue)
            _ -> Nothing
        _ -> Nothing

  -- | Add transactions to the corpus discarding reverted ones
  addToCorpus :: Int -> [(Tx, (VMResult, Gas))] -> Corpus -> Corpus
  addToCorpus n res corpus =
    if null rtxs then corpus else Set.insert (n, rtxs) corpus
    where rtxs = fst <$> res

-- | Execute a transaction, capturing the PC and codehash of each instruction
-- executed, saving the transaction if it finds new coverage.
execTxOptC
  :: (MonadIO m, MonadReader Env m, MonadState (VM, Campaign) m, MonadThrow m)
  => Tx
  -> m (VMResult, Gas)
execTxOptC tx = do
  (vm, camp@Campaign{coverage = oldCov}) <- get
  ((res, (cov', grew)), vm') <- runStateT (execTxWithCov tx oldCov) vm
  put (vm', camp { coverage = cov' })
  when grew $ do
    let dict' = case tx.call of
          SolCall c -> gaddCalls (Set.singleton c) camp.genDict
          _ -> camp.genDict
    modify' $ \(_vm, c) -> (_vm, c { newCoverage = True, genDict = dict' })
  pure res

-- | Given current `gasInfo` and a sequence of executed transactions, updates
-- information on highest gas usage for each call
updateGasInfo
  :: [(Tx, (VMResult, Gas))]
  -> [Tx]
  -> Map Text (Gas, [Tx])
  -> Map Text (Gas, [Tx])
updateGasInfo [] _ gi = gi
updateGasInfo ((tx@Tx{call = SolCall (f, _)}, (_, used')):txs) tseq gi =
  case mused of
    Nothing -> rec
    Just (used, _) | used' > used -> rec
    Just (used, otseq) | (used' == used) && (length otseq > length tseq') -> rec
    _ -> updateGasInfo txs tseq' gi
  where mused = Map.lookup f gi
        tseq' = tx:tseq
        rec   = updateGasInfo txs tseq' (Map.insert f (used', reverse tseq') gi)
updateGasInfo ((t, _):ts) tseq gi = updateGasInfo ts (t:tseq) gi

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list
-- of transactions, constantly checking if we've solved any tests or can shrink
-- known solves.
evalSeq
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m, MonadState (VM, Campaign) m)
  => VM
  -> (Tx -> m a)
  -> [Tx]
  -> m [(Tx, a)]
evalSeq vmForShrink e = go [] where
  go r xs = do
    (v', camp) <- get
    camp' <- execStateT (runUpdate (updateTest vmForShrink (v', reverse r))) camp
    put (v', camp')
    case xs of
      []     -> pure []
      (y:ys) -> e y >>= \a -> ((y, a) :) <$> go (y:r) ys

-- | Given a rule for updating a particular test's state, apply it to each test
-- in a 'Campaign'.
runUpdate
  :: (MonadReader Env m, MonadState Campaign m)
  => (EchidnaTest -> m EchidnaTest)
  -> m ()
runUpdate f = do
  tests' <- mapM f =<< gets (.tests)
  modify' $ \c -> c { tests = tests' }

-- | Given an initial 'VM' state and a @('SolTest', 'TestState')@ pair, as well
-- as possibly a sequence of transactions and the state after evaluation, see if:
-- (0): The test is past its 'testLimit' or 'shrinkLimit' and should be presumed un[solve|shrink]able
-- (1): The test is 'Open', and this sequence of transactions solves it
-- (2): The test is 'Open', and evaluating it breaks our runtime
-- (3): The test is unshrunk, and we can shrink it
-- Then update accordingly, keeping track of how many times we've tried to solve or shrink.
updateTest
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m)
  => VM
  -> (VM, [Tx])
  -> EchidnaTest
  -> m EchidnaTest
updateTest vmForShrink (vm, xs) test = do
  limit <- asks (.cfg.campaignConf.testLimit)
  dappInfo <- asks (.dapp)
  case test.state of
    Open i | i > limit -> case test.testType of
      OptimizationTest _ _ -> pure $ test { state = Large (-1) }
      _                    -> pure $ test { state = Passed }
    Open i -> do
      (testValue, vm') <- evalStateT (checkETest test) vm
      let events = extractEvents False dappInfo vm'
      let results = getResultFromVM vm'
      pure $ updateOpenTest test xs i (testValue, events, results)
    _ ->
      -- TODO: We shrink already in `step`, but we shrink here too. It makes
      -- shrink go faster when some tests are still fuzzed. It's not incorrect
      -- but requires passing `vmForShrink` and feels a bit wrong.
      shrinkTest vmForShrink test
