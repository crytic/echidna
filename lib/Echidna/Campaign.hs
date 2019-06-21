{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Campaign where

import Control.Lens
import Control.Monad (liftM2, replicateM, when)
import Control.Monad.Catch (MonadCatch(..), MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(..), MonadIO, liftIO, StateT, evalStateT, runStateT, execStateT)
import Data.Aeson (ToJSON(..), object)
import Data.Bool (bool)
import Data.Either (lefts)
import Data.Foldable (toList)
import Data.Map (Map, mapKeys, unionWith)
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Ord (comparing)
import Data.Has (Has(..))
import Data.Set (Set, union, elems)
import Data.Text (unpack)
import Data.List (intercalate)
import EVM
import EVM.Types (W256)
import Numeric (showHex)

import Echidna.ABI
import Echidna.Exec
import Echidna.Test
import Echidna.Transaction

-- | Configuration for running an Echidna 'Campaign'.
data CampaignConf = CampaignConf { testLimit     :: Int
                                   -- ^ Maximum number of function calls to execute while fuzzing
                                 , seqLen        :: Int
                                   -- ^ Number of calls between state resets (e.g. \"every 10 calls,
                                   -- reset the state to avoid unrecoverable states/save memory\"
                                 , shrinkLimit   :: Int
                                   -- ^ Maximum number of candidate sequences to evaluate while shrinking
                                 , knownCoverage :: Maybe (Map W256 (Set Int))
                                   -- ^ If applicable, initially known coverage. If this is 'Nothing',
                                   -- Echidna won't collect coverage information (and will go faster)
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

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests    :: [(SolTest, TestState)] -- ^ Tests being evaluated
                         , _coverage :: Map W256 (Set Int)     -- ^ Coverage captured (NOTE: we don't always record this)
                         , _newCoverage :: Bool
                         , _genTrans :: [[Tx]]
                         , _genDict  :: GenDict                -- ^ Generation dictionary
                         }

instance ToJSON Campaign where
  toJSON (Campaign ts co _ _ _) = object $ ("tests", toJSON $ bimap (unpack . fst) toJSON <$> ts)
    : if co == mempty then [] else [("coverage size", toJSON $ length $ concat $ map elems $ toList co), ("coverage",) . toJSON . mapKeys (`showHex` "") $ toList <$> co]

makeLenses ''Campaign

instance Semigroup Campaign where
  (Campaign t c b x g) <> (Campaign t' c' b' x' g') = Campaign (t <> t') (c <> c') (b || b')  (x <> x') (g <> g')

instance Monoid Campaign where
  mempty = Campaign mempty mempty False mempty mempty

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone (Campaign ts _ _ _ _) = view (hasLens . to (liftM2 (,) testLimit shrinkLimit)) <&> \(tl, sl) ->
  all (\case Open i -> i >= tl; Large i _ -> i >= sl; _ -> True) $ snd <$> ts

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccess :: Campaign -> Bool
isSuccess (Campaign ts _ _ _ _) =
  all (\case { Passed -> True; Open _ -> True; _ -> False; }) $ snd <$> ts

-- | Given an initial 'VM' state and a @('SolTest', 'TestState')@ pair, as well as possibly a sequence
-- of transactions and the state after evaluation, see if:
-- (0): The test is past its 'testLimit' or 'shrinkLimit' and should be presumed un[solve|shrink]able
-- (1): The test is 'Open', and this sequence of transactions solves it
-- (2): The test is 'Open', and evaluating it breaks our runtime
-- (3): The test is unshrunk, and we can shrink it
-- Then update accordingly, keeping track of how many times we've tried to solve or shrink.
updateTest :: ( MonadCatch m, MonadRandom m, MonadReader x m, Has TestConf x, Has CampaignConf x)
           => VM -> Maybe (VM, [Tx]) -> (SolTest, TestState) -> m (SolTest, TestState)
updateTest v (Just (v', xs)) (n, t) = view (hasLens . to testLimit) >>= \tl -> (n,) <$> case t of
  Open i    | i >= tl -> pure Passed
  Open i              -> catch (evalStateT (checkETest n) v' <&> bool (Large (-1) xs) (Open (i + 1)))
                               (pure . Failed)
  _                   -> snd <$> updateTest v Nothing (n,t)
updateTest v Nothing (n, t) = view (hasLens . to shrinkLimit) >>= \sl -> (n,) <$> case t of
  Large i x | i >= sl -> pure $ Solved x
  Large i x           -> if length x > 1 || any canShrinkTx x
                           then Large (i + 1) <$> evalStateT (shrinkSeq n x) v
                           else pure $ Solved x
  _                   -> pure t

-- | Given a rule for updating a particular test's state, apply it to each test in a 'Campaign'.
runUpdate :: (MonadState x m, Has Campaign x) => ((SolTest, TestState) -> m (SolTest, TestState)) -> m ()
runUpdate f = use (hasLens . tests) >>= mapM f >>= (hasLens . tests .=)

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list of transactions, constantly
-- checking if we've solved any tests or can shrink known solves.
evalSeq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has TestConf x, Has CampaignConf x, Has Campaign y, Has VM y)
        => VM -> (Tx -> m VMResult) -> [Tx] -> m [Tx]
evalSeq v e = go [] where
  go r xs = use hasLens >>= \v' -> runUpdate (updateTest v $ Just (v',reverse r)) >>
    case xs of []     -> return r
               (y:ys) -> (do
                           vm <- e y
                           if ( classifyRes vm == ResRevert)
                            then go r ys
                            else go (y:r) ys
                         )

-- | Execute a transaction, capturing the PC and codehash of each instruction executed, saving the
-- transaction if it finds new coverage.
execTxOptC :: (MonadState x m, Has Campaign x, Has VM x, MonadThrow m) => Tx -> m VMResult
execTxOptC t = do
  og  <- hasLens . coverage <<.= mempty
  res <- execTxWith vmExcept (usingCoverage $ pointCoverage (hasLens . coverage)) t
  hasLens . coverage %= unionWith union og
  grew <- (== LT) . comparing coveragePoints og <$> use (hasLens . coverage)
  --when grew $ hasLens . genDict %= gaddCalls (lefts [t ^. call])
  when grew $ hasLens . newCoverage .= True
  return res

ppTxs :: [Tx] -> String
ppTxs txs = intercalate " " $ map (\(Tx c _ _ _ ) -> either ppSolCall (const "<CREATE>") c) txs

insertAt :: a -> [a] -> Int -> [a]
insertAt v [] n = [v]
insertAt v arr 1 = (v:arr)
insertAt v (x:xs) n = (x:(insertAt v xs $ n - 1))

insertAtRandom :: MonadRandom m => [a] -> [a] -> m [a]
insertAtRandom xs [] = return xs
insertAtRandom xs (y:ys) = do idx <- getRandomR (0, (length xs) - 1) 
                              insertAtRandom (insertAt y xs idx) ys


randseq :: ( MonadCatch m, MonadRandom m, MonadIO m,  MonadReader x m, MonadState y m
           , Has TestConf x, Has CampaignConf x, Has Campaign y)
        => Int -> World -> m [Tx]

randseq ql w = do ca <- use hasLens
                  n <- getRandomR (0, 4 :: Integer)
                  let gts = ca ^. genTrans
                  rtxs <- replicateM ql (evalStateT genTxM (w, ca ^. genDict))
                  case (n, gts) of
                              -- random generation of transactions
                    (_, []) -> return rtxs
                    (0, _ ) -> return rtxs
                              -- concatenate rare sequences
                    (1, _ ) -> do idxs <- sequence $ replicate 10 (getRandomR (0, (length gts) - 1))
                                  return $ take (10*ql) $ concatMap (gts !!) idxs
                              -- mutate a rare sequence
                    (2, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                  sequence $ map mutTx $ gts !! idx
                              -- shrink a rare sequence
                    (3, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                  sequence $ map shrinkTx $ gts !! idx
                              -- randomly insert transactions into a rare sequence
                    (_, _ ) -> do idx <- getRandomR (0, (length gts) - 1)
                                  n <- getRandomR (0, 10)
                                  insertAtRandom (gts !! idx) (take n rtxs) 
                   

                --2 -> undefined
                --3 -> undefined 

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves. Update coverage as a result
callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadIO m, MonadState y m
           , Has TestConf x, Has CampaignConf x, Has Campaign y)
        => VM -> World -> Int -> m ()
callseq v w ql = do
  ef <- bool execTx execTxOptC . isNothing . knownCoverage <$> view hasLens
  hasLens . newCoverage .= False
  ca <- use hasLens
  --r <- getRandomR (1 , 10 :: Int) 
  is <- randseq ql w --if ((length $ ca ^. genTrans) <= 1 || r <= 9) then replicateM ql (evalStateT genTxM (w, ca ^. genDict)) else randseq ql (ca ^. genTrans)
  (rtxs, (_, ca')) <- runStateT (evalSeq v ef is) (v, ca)
  assign hasLens ca'
  when (ca' ^. newCoverage) $ hasLens . genTrans %= ((reverse rtxs):)
  when (ca' ^. newCoverage) $ liftIO $ print $ ppTxs $ reverse rtxs -- $ length $ ca' ^. genTrans
  --ca'' <- use hasLens
  --assign hasLens ca''

-- | Run a fuzzing campaign given an initial universe state, some tests, and an optional dictionary
-- to generate calls with. Return the 'Campaign' state once we can't solve or shrink anything.
campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadIO m, Has TestConf x, Has CampaignConf x)
         => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
         -> VM                  -- ^ Initial VM state
         -> World               -- ^ Initial world state
         -> [SolTest]           -- ^ Tests to evaluate
         -> Maybe GenDict       -- ^ Optional generation dictionary
         -> m Campaign
campaign u v w ts d = let d' = fromMaybe mempty d in fmap (fromMaybe mempty) (view (hasLens . to knownCoverage))
  >>= \c -> execStateT runCampaign (Campaign ((,Open (-1)) <$> ts) c False mempty d') where
    step        = runUpdate (updateTest v Nothing) >> u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap snd)) >>= update
    update c    = view hasLens >>= \(CampaignConf tl q sl _) ->
      if | any (\case Open  n   -> n < tl; _ -> False) c -> callseq v w q >> step
         | any (\case Large n _ -> n < sl; _ -> False) c -> step
         | otherwise                                     -> u
