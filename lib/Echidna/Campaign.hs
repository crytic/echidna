{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module Echidna.Campaign where

import Control.Lens
import Control.Monad (replicateM, when, unless)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom, RandT, evalRandT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Reader (runReaderT, asks)
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, execStateT, gets, MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Random.Strict (liftCatch)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as H
import Data.Map (Map, unionWith, (\\), elems, keys, lookup, insert, mapWithKey)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import System.Random (mkStdGen)

import EVM (Contract, VM(..), VMResult(..), bytecode)
import qualified EVM (Env(..))
import EVM.ABI (getAbi, AbiType(AbiAddressType), AbiValue(AbiAddress))
import EVM.Types (Addr, Expr(ConcreteBuf))

import Echidna.ABI
import Echidna.Exec
import Echidna.Test
import Echidna.Transaction
import Echidna.Shrink (shrinkTest)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Corpus (InitialCorpus)
import Echidna.Types.Coverage (coveragePoints)
import Echidna.Types.Test
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Signature (makeBytecodeMemo)
import Echidna.Types.Tx (TxCall(..), Tx(..), getResult, call)
import Echidna.Types.World (World)
import Echidna.Mutator.Corpus
import Echidna.Events (extractEvents)

instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM
instance MonadCatch m => MonadCatch (RandT g m) where
  catch = liftCatch catch

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: MonadReader EConfig m => Campaign -> m Bool
isDone c | null c._tests = do
  conf <- asks (.campaignConf)
  pure $ c._ncallseqs * conf.seqLen >= conf.testLimit
isDone c = do
  conf <- asks (.campaignConf)
  let res (Open  i)   = if i >= conf.testLimit then Just True else Nothing
      res Passed      = Just True
      res (Large i)   = if i >= conf.shrinkLimit then Just False else Nothing
      res Solved      = Just False
      res (Failed _)  = Just False
  let testResults = res . (.testState) <$> c._tests
  let done = if conf.stopOnFail then Just False `elem` testResults
                                else all isJust testResults
  pure done

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccessful :: Campaign -> Bool
isSuccessful Campaign{_tests} =
  all (\case { Passed -> True; Open _ -> True; _ -> False; }) ((.testState) <$> _tests)

-- | Given an initial 'VM' state and a @('SolTest', 'TestState')@ pair, as well as possibly a sequence
-- of transactions and the state after evaluation, see if:
-- (0): The test is past its 'testLimit' or 'shrinkLimit' and should be presumed un[solve|shrink]able
-- (1): The test is 'Open', and this sequence of transactions solves it
-- (2): The test is 'Open', and evaluating it breaks our runtime
-- (3): The test is unshrunk, and we can shrink it
-- Then update accordingly, keeping track of how many times we've tried to solve or shrink.
updateTest :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m)
           => VM -> (VM, [Tx]) -> EchidnaTest -> m EchidnaTest
updateTest vmForShrink (vm, xs) test = do
  limit <- asks (.cfg.campaignConf.testLimit)
  dappInfo <- asks (.dapp)
  case test.testState of
    Open i | i >= limit -> case test.testType of
      OptimizationTest _ _ -> pure $ test { testState = Large (-1) }
      _                    -> pure $ test { testState = Passed }
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

-- | Given a rule for updating a particular test's state, apply it to each test in a 'Campaign'.
runUpdate :: (MonadReader Env m, MonadState Campaign m)
          => (EchidnaTest -> m EchidnaTest) -> m ()
runUpdate f = let l = tests in use l >>= mapM f >>= (l .=)

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list of transactions, constantly
-- checking if we've solved any tests or can shrink known solves.
evalSeq :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m, MonadState (VM, Campaign) m)
        => VM -> (Tx -> m a) -> [Tx] -> m [(Tx, a)]
evalSeq vmForShrink e = go [] where
  go r xs = do
    (v', camp) <- get
    camp' <- execStateT (runUpdate (updateTest vmForShrink (v', reverse r))) camp
    put (v', camp')
    case xs of []     -> pure []
               (y:ys) -> e y >>= \a -> ((y, a) :) <$> go (y:r) ys

-- | Given current `gasInfo` and a sequence of executed transactions, updates information on highest
-- gas usage for each call
updateGasInfo :: [(Tx, (VMResult, Int))] -> [Tx] -> Map Text (Int, [Tx]) -> Map Text (Int, [Tx])
updateGasInfo [] _ gi = gi
updateGasInfo ((t@(Tx (SolCall (f, _)) _ _ _ _ _ _), (_, used')):ts) tseq gi =
  case mused of
    Nothing -> rec
    Just (used, _) | used' > used -> rec
    Just (used, otseq) | (used' == used) && (length otseq > length tseq') -> rec
    _ -> updateGasInfo ts tseq' gi
  where mused = Data.Map.lookup f gi
        tseq' = t:tseq
        rec   = updateGasInfo ts tseq' (insert f (used', reverse tseq') gi)
updateGasInfo ((t, _):ts) tseq gi = updateGasInfo ts (t:tseq) gi

-- | Execute a transaction, capturing the PC and codehash of each instruction executed, saving the
-- transaction if it finds new coverage.
execTxOptC :: (MonadIO m, MonadState (VM, Campaign) m, MonadThrow m) => Tx -> m (VMResult, Int)
execTxOptC tx = do
  (vm, Campaign{_bcMemo, _coverage = oldCov}) <- get
  let cov = _2 . coverage
  ((res, newCov), vm') <- runStateT (execTxWithCov _bcMemo tx) vm
  _1 .= vm'
  let vmr = getResult $ fst res
  -- Update the coverage map with the proper binary according to the vm result
  cov .= mapWithKey (\_ s -> Set.map (set _4 vmr) s) newCov
  -- Update the global coverage map with the union of the result just obtained
  cov %= unionWith Set.union oldCov
  grew <- (== LT) . comparing coveragePoints oldCov <$> use cov
  when grew $ do
    case tx.call of
      SolCall c -> _2 . genDict %= gaddCalls (Set.singleton c)
      _ -> pure ()
    _2 . newCoverage .= True
  return res

-- | Given a list of transactions in the corpus, save them discarding reverted transactions
addToCorpus :: MonadState Campaign m => Int -> [(Tx, (VMResult, Int))] -> m ()
addToCorpus n res = unless (null rtxs) $ corpus %= Set.insert (n, rtxs)
  where rtxs = fst <$> res

-- | Generate a new sequences of transactions, either using the corpus or with randomly created transactions
randseq :: (MonadRandom m, MonadReader Env m, MonadState Campaign m)
        => InitialCorpus -> Int -> Map Addr Contract -> World -> m [Tx]
randseq (n,txs) ql o w = do
  ca <- get
  cs <- asks (.cfg.campaignConf.mutConsts)
  txConf <- asks (.cfg.txConf)
  let ctxs = ca._corpus
      -- TODO: include reproducer when optimizing
      --rs   = filter (not . null) $ map (view testReproducer) $ ca ^. tests
      p    = ca._ncallseqs
  if n > p then -- Replay the transactions in the corpus, if we are executing the first iterations
    return $ txs !! p
  else do
    memo <- gets (._bcMemo)
    -- Randomly generate new random transactions
    gtxs <- replicateM ql $ runReaderT (genTxM memo o) (w, txConf)
    -- Generate a random mutator
    cmut <- if ql == 1 then seqMutatorsStateless (fromConsts cs) else seqMutatorsStateful (fromConsts cs)
    -- Fetch the mutator
    let mut = getCorpusMutation cmut
    if Set.null ctxs then
      return gtxs      -- Use the generated random transactions
    else
      mut ql ctxs gtxs -- Apply the mutator

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves. Update coverage as a result
callseq :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m, MonadState Campaign m)
        => InitialCorpus -> VM -> World -> Int -> m ()
callseq ic v w ql = do
  conf <- asks (.cfg.campaignConf)
  -- First, we figure out whether we need to execute with or without coverage optimization and gas info,
  -- and pick our execution function appropriately
  let coverageEnabled = isJust conf.knownCoverage
  let ef = if coverageEnabled then execTxOptC else (\t -> do (xd, ca) <- get
                                                             (r, vm') <- runStateT (execTx t) xd
                                                             put (vm', ca)
                                                             pure r)
      old = v._env._contracts
  let gasEnabled = conf.estimateGas
  -- Then, we get the current campaign state
  ca <- get
  -- Then, we generate the actual transaction in the sequence
  is <- randseq ic ql old w
  -- We then run each call sequentially. This gives us the result of each call, plus a new state
  (res, (vm, camp)) <- runStateT (evalSeq v ef is) (v, ca)
  let new = vm._env._contracts
      -- compute the addresses not present in the old VM via set difference
      diff = keys $ new \\ old
      -- and construct a set to union to the constants table
      diffs = H.fromList [(AbiAddressType, Set.fromList $ AbiAddress <$> diff)]
  -- Save the global campaign state (also vm state, but that gets reset before it's used)
  put camp -- Update the gas estimation
  when gasEnabled $ gasInfo %= updateGasInfo res []
  -- If there is new coverage, add the transaction list to the corpus
  when (camp._newCoverage) $ addToCorpus (camp._ncallseqs + 1) res
  -- Reset the new coverage flag
  newCoverage .= False
  -- Keep track of the number of calls to `callseq`
  ncallseqs += 1
  -- Now we try to parse the return values as solidity constants, and add then to the 'GenDict'
  types <- gets (._genDict.rTypes)
  let results = parse (map (\(t, (vr, _)) -> (t, vr)) res) types
      -- union the return results with the new addresses
      additions = H.unionWith Set.union diffs results
  -- append to the constants dictionary
  let dict = camp._genDict
  genDict .= dict
    { constants = H.unionWith Set.union additions dict.constants
    , dictValues = Set.union (mkDictValues $ Set.unions $ H.elems additions) dict.dictValues
    }
  where
    -- Given a list of transactions and a return typing rule, this checks whether we know the return
    -- type for each function called, and if we do, tries to parse the return value as a value of that
    -- type. It returns a 'GenDict' style HashMap.
    parse l rt = H.fromList . flip mapMaybe l $ \(tx, result) -> do
      fname <- case tx.call of
        SolCall (fname, _) -> Just fname
        _ -> Nothing
      type' <- rt fname
      case result of
        VMSuccess (ConcreteBuf buf) ->
          case runGetOrFail (getAbi type') (LBS.fromStrict buf) of
            Right (_, _, abiValue) -> Just (type', Set.singleton abiValue)
            _ -> Nothing
        _ -> Nothing

-- | Run a fuzzing campaign given an initial universe state, some tests, and an optional dictionary
-- to generate calls with. Return the 'Campaign' state once we can't solve or shrink anything.
campaign
  :: (MonadIO m, MonadCatch m, MonadRandom m, MonadReader Env m)
  => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
  -> VM                  -- ^ Initial VM state
  -> World               -- ^ Initial world state
  -> [EchidnaTest]       -- ^ Tests to evaluate
  -> Maybe GenDict       -- ^ Optional generation dictionary
  -> [[Tx]]              -- ^ Initial corpus of transactions
  -> m Campaign
campaign u vm w ts d txs = do
  conf <- asks (.cfg.campaignConf)
  let c = fromMaybe mempty conf.knownCoverage
  let effectiveSeed = fromMaybe d'.defSeed conf.seed
      effectiveGenDict = d' { defSeed = effectiveSeed }
      d' = fromMaybe defaultDict d
  execStateT
    (evalRandT runCampaign (mkStdGen effectiveSeed))
    (Campaign ts c mempty effectiveGenDict False Set.empty 0 memo)
  where
    -- "mapMaybe ..." is to get a list of all contracts
    memo        = makeBytecodeMemo . mapMaybe (viewBuffer . (^. bytecode)) . elems $ vm._env._contracts
    runCampaign = gets (fmap (.testState) . (._tests)) >>= update
    update c    = do
      let ic = (length txs, txs)
      CampaignConf{testLimit, stopOnFail, seqLen, shrinkLimit} <- asks (.cfg.campaignConf)
      Campaign{_ncallseqs} <- get
      if | stopOnFail && any (\case Solved -> True; Failed _ -> True; _ -> False) c ->
           lift u
         | any (\case Open  n   -> n < testLimit; _ -> False) c ->
           callseq ic vm w seqLen >> step
         | any (\case Large n   -> n < shrinkLimit; _ -> False) c ->
           step
         | null c && (seqLen * _ncallseqs) < testLimit ->
           callseq ic vm w seqLen >> step
         | otherwise ->
           lift u
    step = runUpdate (shrinkTest vm) >> lift u >> runCampaign
