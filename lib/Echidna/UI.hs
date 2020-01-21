{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.UI where

import Brick
import Brick.BChan
import Control.Lens
import Control.Monad (liftM2, liftM3, void, when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader)
import Control.Monad.Random.Strict (MonadRandom)
import Data.Has (Has(..))
import Data.Maybe (maybe, fromMaybe)
import EVM (VM)
import Graphics.Vty (Event(..), Key(..), Modifier(..), defaultConfig, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (killThread, forkIO)

import Echidna.Campaign
import Echidna.ABI
import Echidna.Solidity
import Echidna.Test
import Echidna.Transaction
import Echidna.UI.Report
import Echidna.UI.Widgets

data UIConf = UIConf { _dashboard :: Bool
                     , _maxTime   :: Maybe Int
                     , _finished  :: Campaign -> Int -> String
                     }

makeLenses ''UIConf

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
        => m (App (Campaign, UIState) Campaign ())
monitor = let
  cs :: (CampaignConf, Names, TxConf) -> (Campaign, UIState) -> Widget ()
  cs s c = runReader (campaignStatus c) s

  se _ (AppEvent c') = continue (c', Running)
  se c (VtyEvent (EvKey KEsc _))                         = halt c
  se c (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = halt c
  se c _                                                 = continue c
  in
    liftM3 (,,) (view hasLens) (view hasLens) (view hasLens) <&> \s ->
      App (pure . cs s) neverShowCursor se pure (const attrs)

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: MonadIO m => m Bool
isTerminal = liftIO $ (&&) <$> queryTerminal (Fd 0) <*> queryTerminal (Fd 1)

-- | Set up and run an Echidna 'Campaign' while drawing the dashboard, then print 'Campaign' status
-- once done.
ui :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadUnliftIO m
      , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x, Has Names x, Has TxConf x, Has UIConf x)
   => VM        -- ^ Initial VM state
   -> World     -- ^ Initial world state
   -> [SolTest] -- ^ Tests to evaluate
   -> Maybe GenDict
   -> m Campaign
ui v w ts d = let xfer bc = use hasLens >>= liftIO . writeBChan bc
                  d' = fromMaybe defaultDict d
                  getSeed = view $ hasLens . to seed . non (d' ^. defSeed) in do
  bc <- liftIO $ newBChan 100
  t    <- forkIO (void $ campaign (xfer bc) v w ts d)
  dash <- liftM2 (&&) isTerminal $ view (hasLens . dashboard)
  done <- views hasLens $ \cc -> flip runReader (cc :: CampaignConf) . isDone
  app  <- customMain (mkVty defaultConfig) (Just bc) <$> monitor
  time <- views (hasLens . maxTime) . maybe (fmap Just) $ timeout . (* 1000000)
  res  <-  liftIO . time $ if dash
    then fst <$> app (defaultCampaign, Uninitialized)
    else let go = readBChan bc >>= \c -> if done c then pure c else go in go
  final <- maybe (do c <- liftIO (readBChan bc)
                     killThread t
                     when dash . liftIO . void $ app (c, Timedout)
                     return c)
                 pure res
  liftIO . putStrLn =<< view (hasLens . finished) <*> pure final <*> getSeed
  return final
