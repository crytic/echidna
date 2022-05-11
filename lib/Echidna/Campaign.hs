{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Campaign where

import Control.Lens
import Control.Monad (liftM3, replicateM, when, (<=<), ap, unless)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom, RandT, evalRandT, getRandomR, uniform, uniformMay)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, execStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Random.Strict (liftCatch)
import Data.Binary.Get (runGetOrFail)
import Data.Bool (bool)
import Data.Map (Map, unionWith, (\\), elems, keys, lookup, insert, mapWithKey)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Ord (comparing)
import Data.Has (Has(..))
import Data.Text (Text)
import EVM
import EVM.Dapp (DappInfo)
import EVM.ABI (getAbi, AbiType(AbiAddressType), AbiValue(AbiAddress))
import EVM.Types (Addr, Buffer(..))
import System.Random (mkStdGen)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import qualified Data.Set as DS

import Echidna.ABI
import Echidna.Exec
import Echidna.Test
import Echidna.Transaction
import Echidna.Shrink (shrinkSeq)
import Echidna.Types.Campaign
import Echidna.Types.Corpus (InitialCorpus)
import Echidna.Types.Coverage (coveragePoints)
import Echidna.Types.Test
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Signature (makeBytecodeMemo)
import Echidna.Types.Tx (TxCall(..), Tx(..), TxConf, getResult, src, call, _SolCall)
import Echidna.Types.Solidity (SolConf(..), sender)
import Echidna.Types.World (World)
import Echidna.Mutator.Corpus

instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM
instance MonadCatch m => MonadCatch (RandT g m) where
  catch = liftCatch catch

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone c | null (view tests c) = do
  tl <- view (hasLens . testLimit)
  q <- view (hasLens . seqLen)
  return $ view ncallseqs c * q >= tl
isDone (view tests -> ts) = do
  (tl, sl, sof) <- view (hasLens . to (liftM3 (,,) _testLimit _shrinkLimit _stopOnFail))
  let res (Open  i)   = if i >= tl then Just True else Nothing
      res Passed      = Just True
      res (Large i)   = if i >= sl then Just False else Nothing
      res Solved      = Just False
      res (Failed _)  = Just False
  pure $ res . view testState <$> ts & if sof then elem $ Just False else all isJust

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccess :: Campaign -> Bool
isSuccess = allOf (tests . traverse . testState) (\case { Passed -> True; Open _ -> True; _ -> False; })

-- | Given an initial 'VM' state and a @('SolTest', 'TestState')@ pair, as well as possibly a sequence
-- of transactions and the state after evaluation, see if:
-- (0): The test is past its 'testLimit' or 'shrinkLimit' and should be presumed un[solve|shrink]able
-- (1): The test is 'Open', and this sequence of transactions solves it
-- (2): The test is 'Open', and evaluating it breaks our runtime
-- (3): The test is unshrunk, and we can shrink it
-- Then update accordingly, keeping track of how many times we've tried to solve or shrink.
updateTest :: ( MonadCatch m, MonadRandom m, MonadReader x m
              , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has DappInfo x)
           => World -> VM -> Maybe (VM, [Tx]) -> EchidnaTest -> m EchidnaTest


updateTest w vm (Just (vm', xs)) test = do
  tl <- view (hasLens . testLimit)
  case test ^. testState of
    Open i | i >= tl -> case test ^. testType of
                          OptimizationTest _ _ -> pure $ test { _testState = Large (-1) }
                          _                    -> pure $ test { _testState = Passed }
    Open i           -> do r <- evalStateT (checkETest test) vm' 
                           pure $ updateOpenTest test xs i r 
    _                -> updateTest w vm Nothing test

updateTest _ vm Nothing test = do
  sl <- view (hasLens . shrinkLimit)
  let es = test ^. testEvents
      res = test ^. testResult
      x = test ^. testReproducer
      v = test ^. testValue
  case test ^. testState of
    Large i | i >= sl -> pure $ test { _testState =  Solved, _testReproducer = x }
    Large i           -> if length x > 1 || any canShrinkTx x
                             then do (txs, val, evs, r) <- evalStateT (shrinkSeq (checkETest test) (v, es, res) x) vm
                                     pure $ test { _testState = Large (i + 1), _testReproducer = txs, _testEvents = evs, _testResult = r, _testValue = val} 
                             else pure $ test { _testState = Solved, _testReproducer = x}
    _                   -> pure test


-- | Given a rule for updating a particular test's state, apply it to each test in a 'Campaign'.
runUpdate :: (MonadReader x m, Has TxConf x, MonadState y m, Has Campaign y)
          => (EchidnaTest -> m EchidnaTest) -> m ()
runUpdate f = let l = hasLens . tests in use l >>= mapM f >>= (l .=)

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list of transactions, constantly
-- checking if we've solved any tests or can shrink known solves.
evalSeq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has DappInfo x
           , Has Campaign y, Has VM y)
        => World -> VM -> (Tx -> m a) -> [Tx] -> m [(Tx, a)]
evalSeq w v e = go [] where
  go r xs = do
    v' <- use hasLens
    runUpdate (updateTest w v $ Just (v', reverse r))
    case xs of []     -> pure []
               (y:ys) -> e y >>= \a -> ((y, a) :) <$> go (y:r) ys

-- | Given a call sequence that produces Tx with gas >= g for f, try to randomly generate
-- a smaller one that achieves at least that gas usage
shrinkGasSeq :: ( MonadRandom m, MonadReader x m, MonadThrow m
                , Has SolConf x, Has TestConf x, Has TxConf x, MonadState y m, Has VM y)
          => Text -> Int -> [Tx] -> m [Tx]
shrinkGasSeq f g xs = sequence [shorten, shrunk] >>= uniform >>= ap (fmap . flip bool xs) check where
  callsF f' t = t ^? call . _SolCall . _1 == Just f'
  check xs' | callsF f $ last xs' = do
    res <- traverse execTx xs'
    pure $ (snd . head) res >= g
  check _ = pure False
  shrinkSender x = do
    l <- view (hasLens . sender)
    case ifind (const (== x ^. src)) l of
      Nothing     -> pure x
      Just (i, _) -> flip (set src) x . fromMaybe (x ^. src) <$> uniformMay (l ^.. folded . indices (< i))
  shrunk = mapM (shrinkSender <=< shrinkTx) xs
  shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)

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
execTxOptC :: (MonadState x m, Has Campaign x, Has VM x, MonadThrow m) => Tx -> m (VMResult, Int)
execTxOptC t = do
  let cov = hasLens . coverage
  og   <- cov <<.= mempty
  memo <- use $ hasLens . bcMemo
  res  <- execTxWith vmExcept (execTxWithCov memo cov) t
  let vmr = getResult $ fst res
  -- Update the coverage map with the proper binary according to the vm result
  cov %= mapWithKey (\_ s -> DS.map (set _4 vmr) s)
  -- Update the global coverage map with the union of the result just obtained
  cov %= unionWith DS.union og
  grew <- (== LT) . comparing coveragePoints og <$> use cov
  when grew $ do
    hasLens . genDict %= gaddCalls ([t ^. call] ^.. traverse . _SolCall)
    hasLens . newCoverage .= True
  return res

-- | Given a list of transactions in the corpus, save them discarding reverted transactions
addToCorpus :: (MonadState s m, Has Campaign s) => Int -> [(Tx, (VMResult, Int))] -> m ()
addToCorpus n res = unless (null rtxs) $ hasLens . corpus %= DS.insert (toInteger n, rtxs)
  where rtxs = fst <$> res

-- | Generate a new sequences of transactions, either using the corpus or with randomly created transactions
randseq :: ( MonadRandom m, MonadReader x m, MonadState y m
           , Has TxConf x, Has TestConf x, Has CampaignConf x, Has GenDict y, Has Campaign y)
        => InitialCorpus -> Int -> Map Addr Contract -> World -> m [Tx]
randseq (n,txs) ql o w = do
  ca <- use hasLens
  cs <- view $ hasLens . mutConsts
  txConf :: TxConf <- view hasLens
  let ctxs = ca ^. corpus
      -- TODO: include reproducer when optimizing
      --rs   = filter (not . null) $ map (view testReproducer) $ ca ^. tests
      p    = ca ^. ncallseqs
  if n > p then -- Replay the transactions in the corpus, if we are executing the first iterations
    return $ txs !! p
  else do
    memo <- use $ hasLens . bcMemo
    -- Randomly generate new random transactions
    gtxs <- replicateM ql $ runReaderT (genTxM memo o) (w, txConf)
    -- Generate a random mutator
    cmut <- if ql == 1 then seqMutatorsStateless (fromConsts cs) else seqMutatorsStateful (fromConsts cs)
    -- Fetch the mutator
    let mut = getCorpusMutation cmut
    if DS.null ctxs then
      return gtxs      -- Use the generated random transactions
    else
      mut ql ctxs gtxs -- Apply the mutator

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves. Update coverage as a result
callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has DappInfo x, Has Campaign y, Has GenDict y)
        => InitialCorpus -> VM -> World -> Int -> m ()
callseq ic v w ql = do
  -- First, we figure out whether we need to execute with or without coverage optimization and gas info,
  -- and pick our execution function appropriately
  coverageEnabled <- isJust <$> view (hasLens . knownCoverage)
  let ef = if coverageEnabled then execTxOptC else execTx
      old = v ^. env . EVM.contracts
  gasEnabled <- view $ hasLens . estimateGas
  -- Then, we get the current campaign state
  ca <- use hasLens
  -- Then, we generate the actual transaction in the sequence
  is <- randseq ic ql old w
  -- We then run each call sequentially. This gives us the result of each call, plus a new state
  (res, s) <- runStateT (evalSeq w v ef is) (v, ca)
  let new = s ^. _1 . env . EVM.contracts
      -- compute the addresses not present in the old VM via set difference
      diff = keys $ new \\ old
      -- and construct a set to union to the constants table
      diffs = H.fromList [(AbiAddressType, S.fromList $ AbiAddress <$> diff)]
  -- Save the global campaign state (also vm state, but that gets reset before it's used)
  hasLens .= snd s -- Update the gas estimation
  when gasEnabled $ hasLens . gasInfo %= updateGasInfo res []
  -- If there is new coverage, add the transaction list to the corpus
  when (s ^. _2 . newCoverage) $ addToCorpus (s ^. _2 . ncallseqs + 1) res
  -- Reset the new coverage flag
  hasLens . newCoverage .= False
  -- Keep track of the number of calls to `callseq`
  hasLens . ncallseqs += 1
  -- Now we try to parse the return values as solidity constants, and add then to the 'GenDict'
  types <- use $ hasLens . rTypes
  let results = parse (map (\(t, (vr, _)) -> (t, vr)) res) types
      -- union the return results with the new addresses
      additions = H.unionWith S.union diffs results
  -- append to the constants dictionary
  modifying (hasLens . genDict . constants) . H.unionWith S.union $ additions
  where
    -- Given a list of transactions and a return typing rule, this checks whether we know the return
    -- type for each function called, and if we do, tries to parse the return value as a value of that
    -- type. It returns a 'GenDict' style HashMap.
    parse l rt = H.fromList . flip mapMaybe l $ \(x, r) -> case (rt =<< x ^? call . _SolCall . _1, r) of
      (Just ty, VMSuccess (ConcreteBuffer b)) ->
        (ty,) . S.fromList . pure <$> runGetOrFail (getAbi ty) (b ^. lazy) ^? _Right . _3
      _ -> Nothing

-- | Run a fuzzing campaign given an initial universe state, some tests, and an optional dictionary
-- to generate calls with. Return the 'Campaign' state once we can't solve or shrink anything.
campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m
            , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has DappInfo x)
         => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
         -> VM                  -- ^ Initial VM state
         -> World               -- ^ Initial world state
         -> [EchidnaTest]       -- ^ Tests to evaluate
         -> Maybe GenDict       -- ^ Optional generation dictionary
         -> [[Tx]]              -- ^ Initial corpus of transactions
         -> m Campaign
campaign u vm w ts d txs = do
  c <- fromMaybe mempty <$> view (hasLens . knownCoverage)
  g <- view (hasLens . seed)
  let effectiveSeed = fromMaybe (d' ^. defSeed) g
      effectiveGenDict = d' { _defSeed = effectiveSeed }
      d' = fromMaybe defaultDict d
  execStateT
    (evalRandT runCampaign (mkStdGen effectiveSeed))
    (Campaign
      ts
      c
      mempty
      effectiveGenDict
      False
      DS.empty
      0
      memo
    )
  where
    -- "mapMaybe ..." is to get a list of all contracts
    ic          = (length txs, txs)
    memo        = makeBytecodeMemo . mapMaybe (viewBuffer . (^. bytecode)) . elems $ (vm ^. env . EVM.contracts)
    step        = runUpdate (updateTest w vm Nothing) >> lift u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap (view testState))) >>= update
    update c    = do
      CampaignConf tl sof _ q sl _ _ _ _ _ <- view hasLens
      Campaign { _ncallseqs } <- view hasLens <$> get
      if | sof && any (\case Solved -> True; Failed _ -> True; _ -> False) c -> lift u
         | any (\case Open  n   -> n < tl; _ -> False) c                       -> callseq ic vm w q >> step
         | any (\case Large n   -> n < sl; _ -> False) c                       -> step
         | null c && (q * _ncallseqs) < tl                                     -> callseq ic vm w q >> step
         | otherwise                                                           -> lift u
