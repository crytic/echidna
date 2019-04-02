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
import Control.Monad.State.Strict (MonadState(..), MonadIO, liftIO, StateT, evalStateT, execStateT)
import Data.Aeson (ToJSON(..), object)
import Data.Bool (bool)
import Data.Map (Map, mapKeys)
import Data.Maybe (maybeToList)
import Data.Foldable (toList)
import Data.Has (Has(..))
import Data.Set (Set)
import Data.Text (unpack)
import EVM
import EVM.Types (W256)
import Numeric (showHex)

import Echidna.ABI
import Echidna.Exec
import Echidna.Test
import Echidna.Transaction
--import Echidna.UI (ppTx) 

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
                                 , ethenoMode   :: Bool
                                   -- Print every transaction in a way that Etheno can parse
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
data Campaign = Campaign { _tests    :: [(SolTest, TestState)]     -- ^ Tests being evaluated
                         , _coverage :: Maybe (Map W256 (Set Int)) -- ^ Coverage, if applicable
                         }

instance ToJSON Campaign where
  toJSON (Campaign ts co) = object $ ("tests", toJSON $ bimap (unpack . fst) toJSON <$> ts)
    : maybeToList (("coverage",) . toJSON . mapKeys (`showHex` "") . fmap toList <$> co)

makeLenses ''Campaign

-- | Given a 'Campaign', checks if we can attempt any solves or shrinks without exceeding
-- the limits defined in our 'CampaignConf'.
isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone (Campaign ts _) = view (hasLens . to (liftM2 (,) testLimit shrinkLimit)) <&> \(tl, sl) ->
  all (\case Open i -> i >= tl; Large i _ -> i >= sl; _ -> True) $ snd <$> ts

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccess :: Campaign -> Bool
isSuccess (Campaign ts _) =
  any (\case { Passed -> True; Open _ -> True; _ -> False; }) $ snd <$> ts

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
        => VM -> (Tx -> m a) -> [Tx] -> m ()
evalSeq v e = go [] where
  go r xs = use hasLens >>= \v' -> runUpdate (updateTest v $ Just (v',reverse r)) >>
    case xs of []     -> pure ()
               (y:ys) -> e y >> go (y:r) ys

-- | Given an initial 'VM' and 'World' state and a number of calls to generate, generate that many calls,
-- constantly checking if we've solved any tests or can shrink known solves.
callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m, MonadIO m
           , Has GenConf x, Has TestConf x, Has CampaignConf x, Has Campaign y, Has Names x)
        => VM -> World -> Int -> Bool -> m (Set Tx)
callseq v w ql False = replicateM ql (evalStateT genTxM w) >>= \is -> use hasLens >>= \ca -> case ca ^. coverage of
  Nothing   -> execStateT (evalSeq v execTx is) (v, ca) >>= assign hasLens . view _2 >> return mempty
  (Just co) -> do (_, co', ca', s) <- execStateT (evalSeq v execTxRecC is) (v, co, ca, mempty :: Set Tx)
                  hasLens .= (ca' & coverage ?~ co')
                  return s

callseq _ w ql True = replicateM ql (evalStateT genTxM w) >>= (mapM ppTx) >>= \is -> liftIO $ print is >> return mempty 

-- | Run a fuzzing campaign given an initial universe state and some tests. Return the 'Campaign' state once
-- we can't solve or shrink anything.
campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadIO m, Has GenConf x, Has TestConf x, Has CampaignConf x, Has Names x)
         => StateT Campaign m a -- ^ Callback to run after each state update (for instrumentation)
         -> VM                  -- ^ Initial VM state
         -> World               -- ^ Initial world state
         -> [SolTest]           -- ^ Tests to evaluate
         -> m Campaign
campaign u v w ts = view (hasLens . to knownCoverage) >>= \c ->
  execStateT runCampaign (Campaign ((,Open (-1)) <$> ts) c) where
    step        = runUpdate (updateTest v Nothing) >> u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap snd)) >>= update
    update c    = view hasLens >>= \(CampaignConf tl q sl _ b) ->
      if | any (\case Open  n   -> n < tl; _ -> False) c -> callseq v w q b >> step
         | any (\case Large n _ -> n < sl; _ -> False) c -> step
         | otherwise                                     -> u
