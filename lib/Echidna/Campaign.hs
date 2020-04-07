{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Campaign where

import Control.Lens
import Control.Monad (liftM3, replicateM, when, (<=<), ap, unless)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Random.Strict (MonadRandom, RandT, evalRandT, getRandomR, uniform, uniformMay, fromList)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, execStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Random.Strict (liftCatch)
import Data.Aeson (ToJSON(..), object)
import Data.Binary.Get (runGetOrFail)
import Data.Bool (bool)
import Data.Map (Map, mapWithKey, mapKeys, unionWith, toList, (\\), keys, lookup, insert)
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Has (Has(..))
import Data.Text (Text)
import Data.Traversable (traverse)
import EVM
import EVM.ABI (getAbi, AbiType(AbiAddressType), AbiValue(AbiAddress))
import EVM.Types (Addr)
import Numeric (showHex)
import System.Random (mkStdGen)

import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as S
import qualified Data.Foldable as DF
import qualified Data.Set as DS

import Echidna.ABI
import Echidna.Exec
import Echidna.Solidity
import Echidna.Test
import Echidna.Transaction
import Echidna.Mutator

instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM
instance MonadCatch m => MonadCatch (RandT g m) where
  catch = liftCatch catch

type MutationConsts = (Integer, Integer, Integer, Integer, Integer)
defaultMutationConsts :: MutationConsts 
defaultMutationConsts = (10,1,1,1,1)

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf { testLimit     :: Int
                                   -- ^ Maximum number of function calls to execute while fuzzing
                                 , stopOnFail    :: Bool
                                   -- ^ Whether to stop the campaign immediately if any property fails
                                 , estimateGas   :: Bool
                                   -- ^ Whether to collect gas usage statistics
                                 , seqLen        :: Int
                                   -- ^ Number of calls between state resets (e.g. \"every 10 calls,
                                   -- reset the state to avoid unrecoverable states/save memory\"
                                 , shrinkLimit   :: Int
                                   -- ^ Maximum number of candidate sequences to evaluate while shrinking
                                 , knownCoverage :: Maybe CoverageMap
                                   -- ^ If applicable, initially known coverage. If this is 'Nothing',
                                   -- Echidna won't collect coverage information (and will go faster)
                                 , seed          :: Maybe Int
                                   -- ^ Seed used for the generation of random transactions
                                 , dictFreq      :: Float
                                   -- ^ Frequency for the use of dictionary values in the random transactions
                                 , corpusDir     :: Maybe FilePath
                                   -- ^ Directory to load and save lists of transactions
                                 , mutConsts     :: MutationConsts
                                 }

-- | State of a particular Echidna test. N.B.: \"Solved\" means a falsifying call sequence was found.
data TestState = Open Int             -- ^ Maybe solvable, tracking attempts already made
               | Large Int [Tx]       -- ^ Solved, maybe shrinable, tracking shrinks tried + best solve
               | Passed               -- ^ Presumed unsolvable
               | Solved [Tx]          -- ^ Solved with no need for shrinking
               | Failed ExecException -- ^ Broke the execution environment
                 deriving Show

instance Eq TestState where
  (Open i)    == (Open j)    = i == j
  (Large i l) == (Large j m) = i == j && l == m
  Passed      == Passed      = True
  (Solved l)  == (Solved m)  = l == m
  _           == _           = False

instance ToJSON TestState where
  toJSON s = object $ ("passed", toJSON passed) : maybeToList desc where
    (passed, desc) = case s of Open _    -> (True, Nothing)
                               Passed    -> (True, Nothing)
                               Large _ l -> (False, Just ("callseq", toJSON l))
                               Solved  l -> (False, Just ("callseq", toJSON l))
                               Failed  e -> (False, Just ("exception", toJSON $ show e))

type Corpus = DS.Set (Integer, [Tx])

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests       :: [(SolTest, TestState)]
                           -- ^ Tests being evaluated
                         , _coverage    :: CoverageMap
                           -- ^ Coverage captured (NOTE: we don't always record this)
                         , _gasInfo     :: Map Text (Int, [Tx])
                           -- ^ Worst case gas (NOTE: we don't always record this)
                         , _genDict     :: GenDict
                           -- ^ Generation dictionary
                         , _newCoverage :: Bool
                           -- ^ Flag to indicate new coverage found
                         , _corpus      :: Corpus
                           -- ^ Set of transactions with maximum coverage
                         , _ncallseqs   :: Integer
                           -- ^ Number of times the callseq is called                         
                         }

instance ToJSON Campaign where
  toJSON (Campaign ts co gi _ _ _ _) = object $ ("tests", toJSON $ mapMaybe format ts)
    : ((if co == mempty then [] else [
    ("coverage",) . toJSON . mapKeys (`showHex` "") $ DF.toList <$> co]) ++
       [(("maxgas",) . toJSON . toList) gi | gi /= mempty]) where
        format (Right _,      Open _) = Nothing
        format (Right (n, _), s)      = Just ("assertion in " <> n, toJSON s)
        format (Left (n, _),  s)      = Just (n,                    toJSON s)

makeLenses ''Campaign

instance Has GenDict Campaign where
  hasLens = genDict

defaultCampaign :: Campaign
defaultCampaign = Campaign mempty mempty mempty defaultDict False mempty 0

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone (view tests -> ts) = view (hasLens . to (liftM3 (,,) testLimit shrinkLimit stopOnFail))
  <&> \(tl, sl, sof) -> let res (Open  i)   = if i >= tl then Just True else Nothing
                            res Passed      = Just True
                            res (Large i _) = if i >= sl then Just False else Nothing
                            res (Solved _)  = Just False
                            res (Failed _)  = Just False
                            in res . snd <$> ts & if sof then elem $ Just False else all isJust

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccess :: Campaign -> Bool
isSuccess (view tests -> ts) =
  all (\case { Passed -> True; Open _ -> True; _ -> False; }) $ snd <$> ts

-- | Given an initial 'VM' state and a @('SolTest', 'TestState')@ pair, as well as possibly a sequence
-- of transactions and the state after evaluation, see if:
-- (0): The test is past its 'testLimit' or 'shrinkLimit' and should be presumed un[solve|shrink]able
-- (1): The test is 'Open', and this sequence of transactions solves it
-- (2): The test is 'Open', and evaluating it breaks our runtime
-- (3): The test is unshrunk, and we can shrink it
-- Then update accordingly, keeping track of how many times we've tried to solve or shrink.
updateTest :: ( MonadCatch m, MonadRandom m, MonadReader x m
              , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x)
           => VM -> Maybe (VM, [Tx]) -> (SolTest, TestState) -> m (SolTest, TestState)
updateTest v (Just (v', xs)) (n, t) = view (hasLens . to testLimit) >>= \tl -> (n,) <$> case t of
  Open i    | i >= tl -> pure Passed
  Open i              -> catch (evalStateT (checkETest n) v' <&> bool (Large (-1) xs) (Open (i + 1)))
                               (pure . Failed)
  _                   -> snd <$> updateTest v Nothing (n,t)
updateTest v Nothing (n, t) = view (hasLens . to shrinkLimit) >>= \sl -> (n,) <$> case t of
  Large i x | i >= sl -> pure $ Solved x
  Large i x           -> if length x > 1 || any canShrinkTx x
                           then Large (i + 1) <$> evalStateT (shrinkSeq (checkETest n) x) v
                           else pure $ Solved x
  _                   -> pure t

-- | Given a rule for updating a particular test's state, apply it to each test in a 'Campaign'.
runUpdate :: (MonadReader x m, Has TxConf x, MonadState y m, Has Campaign y)
          => ((SolTest, TestState) -> m (SolTest, TestState)) -> m ()
runUpdate f = use (hasLens . tests) >>= mapM f >>= (hasLens . tests .=)

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list of transactions, constantly
-- checking if we've solved any tests or can shrink known solves.
evalSeq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has Campaign y, Has VM y)
        => VM -> (Tx -> m a) -> [Tx] -> m [(Tx, a)]
evalSeq v e = go [] where
  go r xs = use hasLens >>= \v' -> runUpdate (updateTest v $ Just (v',reverse r)) >>
    case xs of []     -> pure []
               (y:ys) -> e y >>= \a -> ((y, a) :) <$> go (y:r) ys

-- | Given a call sequence that produces Tx with gas >= g for f, try to randomly generate
-- a smaller one that achieves at least that gas usage
shrinkGasSeq :: ( MonadRandom m, MonadReader x m, MonadThrow m
                , Has SolConf x, Has TestConf x, Has TxConf x, MonadState y m, Has VM y)
          => Text -> Int -> [Tx] -> m [Tx]
shrinkGasSeq f g xs = sequence [shorten, shrunk] >>= uniform >>= ap (fmap . flip bool xs) check where
  callsF f' (Tx(SolCall(f'', _)) _ _ _ _ _ _) = f' == f''
  callsF _ _                                  = False
  check xs' | callsF f $ last xs' = do {_ <- get; res <- traverse execTx xs'; pure ((snd . head) res >= g) }
  check _                         = pure False
  shrinkSender x = view (hasLens . sender) >>= \l -> case ifind (const (== x ^. src)) l of
    Nothing     -> pure x
    Just (i, _) -> flip (set src) x . fromMaybe (x ^. src) <$> uniformMay (l ^.. folded . indices (< i))
  shrunk = mapM (shrinkSender <=< shrinkTx) xs
  shorten = (\i -> take i xs ++ drop (i + 1) xs) <$> getRandomR (0, length xs)

-- | Given current `gasInfo` and a sequence of executed transactions, updates information on highest
-- gas usage for each call
updateGasInfo :: [(Tx, (VMResult, Int))] -> [Tx] -> Map Text (Int, [Tx]) -> Map Text (Int, [Tx])
updateGasInfo [] _ gi = gi
updateGasInfo ((t@(Tx (SolCall(f, _)) _ _ _ _ _ _), (_, used')):ts) tseq gi =
  let mused = Data.Map.lookup f gi
      tseq' = t:tseq
  in  case mused of Nothing -> updateGasInfo ts tseq' (insert f (used', reverse tseq') gi)
                    Just (used, _) | used' > used -> updateGasInfo ts tseq' (insert f (used', reverse tseq') gi)
                    Just (used, otseq) | (used' == used) && (length otseq > length tseq') -> updateGasInfo ts tseq' (insert f (used', reverse tseq') gi)
                    _ -> updateGasInfo ts tseq' gi
updateGasInfo ((t, _):ts) tseq gi = updateGasInfo ts (t:tseq) gi

-- | Execute a transaction, capturing the PC and codehash of each instruction executed, saving the
-- transaction if it finds new coverage.
execTxOptC :: (MonadState x m, Has Campaign x, Has VM x, MonadThrow m) => Tx -> m (VMResult, Int)
execTxOptC t = do
  og  <- hasLens . coverage <<.= mempty
  res <- execTxWith vmExcept (usingCoverage $ pointCoverage (hasLens . coverage)) t
  let vmr = getResult $ fst res
  -- Update the coverage map with the proper binary according to the vm result 
  hasLens . coverage %= mapWithKey (\ _ s -> DS.map (\(i,_) -> (i, vmr)) s)
  -- Update the global coverage map with the union of the result just obtained 
  hasLens . coverage %= unionWith DS.union og
  grew <- (== LT) . comparing coveragePoints og <$> use (hasLens . coverage)
  when grew $ do
    hasLens . genDict %= gaddCalls ([t ^. call] ^.. traverse . _SolCall)
    hasLens . newCoverage .= True
  return res

-- | Given a list of transactions in the corpus, save them discarding reverted transactions
addToCorpus :: (MonadState s m, Has Campaign s) => Integer -> [(Tx, (VMResult, Int))] -> m ()
addToCorpus n res = unless (null rtxs) $ hasLens . corpus %= DS.insert (n, rtxs) where
  rtxs = map fst res

data TxsMutation = TxIdentity
                 | Shrinking
                 | Mutation
                 | Expansion
                 | Swapping
                 | Deletion

data CorpusMutation = Skip
                    | RandomAppend TxsMutation 
                    | RandomPrepend TxsMutation
                    | RandomSplice
                    | RandomInterleave

selectAndMutate :: MonadRandom m 
                => ([Tx] -> m [Tx]) -> Corpus -> m [Tx]
selectAndMutate f ctxs = do
  let somePercent = if (fst . DS.findMax) ctxs > 1   -- if the corpus already contains new elements 
                    then 1 + (DS.size ctxs `div` 20) -- then take 5% of its size
                    else DS.size ctxs                -- otherwise, take all of it
  rtxs <- fromList $ map (\(i, txs) -> (txs, fromInteger i)) $ take somePercent $ DS.toDescList ctxs
  k <- getRandomR (0, length rtxs - 1)
  f $ take k rtxs


selectAndCombine ::  MonadRandom m
                 => ([Tx] -> [Tx] -> m [Tx]) -> Int -> Corpus -> [Tx] -> m [Tx]
selectAndCombine f ql ctxs gtxs = do
          rtxs1 <- fromList $ map (\(i, txs) -> (txs, fromInteger i)) $ DS.toDescList ctxs
          rtxs2 <- fromList $ map (\(i, txs) -> (txs, fromInteger i)) $ DS.toDescList ctxs 
          txs <- f rtxs1 rtxs2
          return . take ql $ txs ++ gtxs

getCorpusMutation :: (MonadThrow m, MonadRandom m, Has GenDict x, MonadState x m) 
                  => CorpusMutation -> Int -> Corpus -> [Tx] -> m [Tx]
getCorpusMutation Skip = \_ _ -> return
getCorpusMutation (RandomAppend m) = 
  case m of
    TxIdentity -> mut return
    Shrinking  -> mut (mapM shrinkTx)
    Mutation   -> mut (mapM mutateTx)
    Expansion  -> mut expandRandList
    Swapping   -> mut swapRandList
    Deletion   -> mut deleteRandList
 where mut f ql ctxs gtxs = do
          rtxs' <- selectAndMutate f ctxs
          return . take ql $ rtxs' ++ gtxs  

getCorpusMutation (RandomPrepend m) = 
  case m of
    TxIdentity -> mut return
    Shrinking  -> mut $ mapM shrinkTx
    Mutation   -> mut $ mapM mutateTx
    Expansion  -> mut expandRandList
    Swapping   -> mut swapRandList
    Deletion   -> mut deleteRandList
 where mut f ql ctxs gtxs = do
          rtxs' <- selectAndMutate f ctxs
          k <- getRandomR (0, ql - 1)
          return . take ql $ take k gtxs ++ rtxs'

getCorpusMutation RandomSplice = selectAndCombine spliceAtRandom
getCorpusMutation RandomInterleave = selectAndCombine interleaveAtRandom

seqMutators :: (MonadRandom m, Has GenDict x, MonadState x m, MonadThrow m) 
            => MutationConsts -> m CorpusMutation
seqMutators (c1, c2, c3, c4, c5) = fromList 
  [(Skip,                     1000),

   (RandomAppend TxIdentity,  fromInteger c1),
   (RandomAppend Shrinking,   fromInteger c2),
   (RandomAppend Mutation,    fromInteger c3),
   (RandomAppend Expansion,   fromInteger c4),
   (RandomAppend Swapping,    fromInteger c4),
   (RandomAppend Deletion,    fromInteger c4),

   (RandomPrepend TxIdentity, fromInteger c1),
   (RandomPrepend Shrinking,  fromInteger c2),
   (RandomPrepend Mutation,   fromInteger c3),
   (RandomPrepend Expansion,  fromInteger c4),
   (RandomPrepend Swapping,   fromInteger c4),
   (RandomPrepend Deletion,   fromInteger c4),

   (RandomSplice,             fromInteger c5),
   (RandomInterleave,         fromInteger c5)
 ]

-- | Generate a new sequences of transactions, either using the corpus or with randomly created transactions
randseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has GenDict y, Has TxConf x, Has TestConf x, Has CampaignConf x, Has Campaign y)
        => Int -> Map Addr Contract -> World -> m [Tx]
randseq ql o w = do
  ca <- use hasLens
  cs <- mutConsts <$> view hasLens
  let ctxs = ca ^. corpus
      p    = fromInteger $ ca ^. ncallseqs
  if length ctxs > p then -- Replay the transactions in the corpus, if we are executing the first iterations
    return $ snd $ DS.elemAt p ctxs
  else
    do
      -- Randomly generate new random transactions
      gtxs <- replicateM ql (evalStateT (genTxM o) (w, ca ^. genDict))
      -- Select a random mutator
      cmut <- seqMutators cs
      let mut = getCorpusMutation cmut
      if DS.null ctxs
      then return gtxs      -- Use the generated random transactions
      else mut ql ctxs gtxs -- Apply the mutator

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves. Update coverage as a result
callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has Campaign y, Has GenDict y)
        => VM -> World -> Int -> m ()
callseq v w ql = do
  -- First, we figure out whether we need to execute with or without coverage optimization and gas info,
  -- and pick our execution function appropriately
  coverageEnabled <- isJust . knownCoverage <$> view hasLens
  let ef = if coverageEnabled then execTxOptC else execTx
      old = v ^. env . EVM.contracts
  gasEnabled <- estimateGas <$> view hasLens
  -- Then, we get the current campaign state
  ca <- use hasLens
  -- Then, we generate the actual transaction in the sequence
  is <- randseq ql old w
  -- We then run each call sequentially. This gives us the result of each call, plus a new state
  (res, s) <- runStateT (evalSeq v ef is) (v, ca)
  let new = s ^. _1 . env . EVM.contracts
      -- compute the addresses not present in the old VM via set difference
      diff = keys $ new \\ old
      -- and construct a set to union to the constants table
      diffs = H.fromList [(AbiAddressType, S.fromList $ AbiAddress <$> diff)]
  -- Save the global campaign state (also vm state, but that gets reset before it's used)
  hasLens .= snd s
  -- Update the gas estimation
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
      (Just ty, VMSuccess b) -> (ty, ) . S.fromList . pure <$> runGetOrFail (getAbi ty) (b ^. lazy) ^? _Right . _3
      _                      -> Nothing


-- | Run a fuzzing campaign given an initial universe state, some tests, and an optional dictionary
-- to generate calls with. Return the 'Campaign' state once we can't solve or shrink anything.
campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m
            , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x)
         => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
         -> VM                  -- ^ Initial VM state
         -> World               -- ^ Initial world state
         -> [SolTest]           -- ^ Tests to evaluate
         -> Maybe GenDict       -- ^ Optional generation dictionary
         -> [[Tx]]              -- ^ Initial corpus of transactions
         -> m Campaign
campaign u v w ts d txs = do
  let d' = fromMaybe defaultDict d
  c <- fromMaybe mempty <$> view (hasLens . to knownCoverage)
  g <- view (hasLens . to seed)
  let g' = mkStdGen $ fromMaybe (d' ^. defSeed) g
  execStateT (evalRandT runCampaign g') (Campaign ((,Open (-1)) <$> ts) c mempty d' False (DS.fromList $ map (1,) txs) 0) where
    step        = runUpdate (updateTest v Nothing) >> lift u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap snd)) >>= update
    update c    = view hasLens >>= \(CampaignConf tl sof _ q sl _ _ _ _ _) ->
      if | sof && any (\case Solved _ -> True; Failed _ -> True; _ -> False) c -> lift u
         | any (\case Open  n   -> n < tl; _ -> False) c                       -> callseq v w q >> step
         | any (\case Large n _ -> n < sl; _ -> False) c                       -> step
         | otherwise                                                           -> lift u
