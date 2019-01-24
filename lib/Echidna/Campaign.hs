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

data CampaignConf = CampaignConf { testLimit     :: Int
                                 , seqLen        :: Int
                                 , shrinkLimit   :: Int
                                 , knownCoverage :: Maybe (Map W256 (Set Int))
                                 }

data TestState = Open Int | Large Int [Tx] | Passed | Solved [Tx] | Failed ExecException deriving Show

data Campaign = Campaign { _tests    :: [(SolTest, TestState)]
                         , _coverage :: Maybe (Map W256 (Set Int))
                         }
makeLenses ''Campaign

isDone :: (MonadReader x m, Has CampaignConf x) => Campaign -> m Bool
isDone (Campaign ts _) = view (hasLens . to (liftM2 (,) testLimit shrinkLimit)) <&> \(tl, sl) ->
  all (\case Open i -> i >= tl; Large i _ -> i >= sl; _ -> True) $ snd <$> ts

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

callseq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has GenConf x, Has TestConf x, Has CampaignConf x, Has Campaign y)
        => VM -> World -> Int -> m (Set Tx)
callseq v w ql = replicateM ql (evalStateT genTxM w) >>= \is -> use hasLens >>= \ca -> case ca ^. coverage of
  Nothing   -> execStateT (evalSeq v execTx is) (v, ca) >>= assign hasLens . view _2 >> return mempty
  (Just co) -> do (_, co', ca', s) <- execStateT (evalSeq v execTxRecC is) (v, co, ca, mempty :: Set Tx)
                  hasLens .= (ca' & coverage ?~ co')
                  return s

campaign :: ( MonadCatch m, MonadRandom m, MonadReader x m, Has GenConf x, Has TestConf x, Has CampaignConf x)
         => StateT Campaign m a -> VM -> World -> [SolTest] -> m Campaign
campaign u v w ts = view (hasLens . to knownCoverage) >>= \c ->
  execStateT runCampaign (Campaign ((,Open (-1)) <$> ts) c) where
    step        = runUpdate (updateTest v Nothing) >> u >> runCampaign
    runCampaign = use (hasLens . tests . to (fmap snd)) >>= update
    update c    = view hasLens >>= \(CampaignConf tl q sl _) ->
      if | any (\case Open  n   -> n < tl; _ -> False) c -> callseq v w q >> step
         | any (\case Large n _ -> n < sl; _ -> False) c -> step
         | otherwise                                     -> u

evalSeq :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadState y m
           , Has TestConf x, Has CampaignConf x, Has Campaign y, Has VM y)
        => VM -> (Tx -> m a) -> [Tx] -> m ()
evalSeq v e l = go [] l where
  go r xs = use hasLens >>= \v' -> runUpdate (updateTest v $ Just (v',reverse r)) >>
    case xs of []     -> pure ()
               (y:ys) -> e y >> go (y:r) ys

runUpdate :: (MonadState x m, Has Campaign x) => ((SolTest, TestState) -> m (SolTest, TestState)) -> m ()
runUpdate f = use (hasLens . tests) >>= mapM f >>= (hasLens . tests .=)
