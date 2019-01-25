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
import Control.Monad (liftM2, replicateM)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Random.Strict (MonadRandom)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState(..), StateT, evalStateT, execStateT)
import Data.Bool (bool)
import Data.Map (Map)
import Data.Has (Has(..))
import Data.Set (Set)
import EVM
import EVM.Types (W256)

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

-- | The state of a fuzzing campaign.
data Campaign = Campaign { _tests    :: [(SolTest, TestState)]     -- ^ Tests being evaluated
                         , _coverage :: Maybe (Map W256 (Set Int)) -- ^ Coverage, if applicable
                         }
makeLenses ''Campaign

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone (Campaign ts _) = view (hasLens . to (liftM2 (,) testLimit shrinkLimit)) <&> \(tl, sl) ->
  all (\case Open i -> i >= tl; Large i _ -> i >= sl; _ -> True) $ snd <$> ts

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
  Large i x           -> if any canShrinkTx x then Large (i + 1) <$> evalStateT (shrinkSeq n x) v
                                              else pure $ Solved x
  _                   -> pure t

-- | Given a rule for updating a particular test's state, apply it to each test in a 'Campaign'.
runUpdate :: (MonadState x m, Has Campaign x) => ((SolTest, TestState) -> m (SolTest, TestState)) -> m ()
runUpdate f = use (hasLens . tests) >>= mapM f >>= (hasLens . tests .=)

-- | Given an initial 'VM' state and a way to run transactions, evaluate a list of transactions, constantly
-- checking if we've solved any tests or can shrink known solves.
evalSeq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has TestConf x, Has CampaignConf x, Has Campaign y, Has VM y)
        => VM -> (Tx -> m a) -> [Tx] -> m ()
evalSeq v e = go [] where
  go r xs = use hasLens >>= \v' -> runUpdate (updateTest v $ Just (v',reverse r)) >>
    case xs of []     -> pure ()
               (y:ys) -> e y >> go (y:r) ys

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves.
callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has GenConf x, Has TestConf x, Has CampaignConf x, Has Campaign y)
        => VM -> World -> Int -> m (Set Tx)
callseq v w ql = replicateM ql (evalStateT genTxM w) >>= \is -> use hasLens >>= \ca -> case ca ^. coverage of
  Nothing   -> execStateT (evalSeq v execTx is) (v, ca) >>= assign hasLens . view _2 >> return mempty
  (Just co) -> do (_, co', ca', s) <- execStateT (evalSeq v execTxRecC is) (v, co, ca, mempty :: Set Tx)
                  hasLens .= (ca' & coverage ?~ co')
                  return s

-- | Run a fuzzing campaign given an initial universe state and some tests. Return the 'Campaign' state once
-- we can't solve or shrink anything.
campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m, Has GenConf x, Has TestConf x, Has CampaignConf x)
         => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
         -> VM                  -- ^ Initial VM state
         -> World               -- ^ Initial world state
         -> [SolTest]           -- ^ Tests to evaluate
         -> m Campaign
campaign u v w ts = view (hasLens . to knownCoverage) >>= \c ->
  execStateT runCampaign (Campaign ((,Open (-1)) <$> ts) c) where
    step        = runUpdate (updateTest v Nothing) >> u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap snd)) >>= update
    update c    = view hasLens >>= \(CampaignConf tl q sl _) ->
      if | any (\case Open  n   -> n < tl; _ -> False) c -> callseq v w q >> step
         | any (\case Large n _ -> n < sl; _ -> False) c -> step
         | otherwise                                     -> u
