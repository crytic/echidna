{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.UI where

import Brick
import Brick.BChan
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad (forever, liftM2, liftM3, void, when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader)
import Control.Monad.Random.Strict (MonadRandom)
import Data.Has (Has(..))
import Data.Maybe (fromMaybe)
import Data.IORef
import EVM (VM)
import Graphics.Vty (Event(..), Key(..), Modifier(..), defaultConfig, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO, forkFinally)
import UnliftIO.Timeout (timeout)

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

data CampaignEvent = CampaignUpdated Campaign | CampaignTimedout Campaign

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
        => m (App (Campaign, UIState) CampaignEvent ())
monitor = let
  cs :: (CampaignConf, Names, TxConf) -> (Campaign, UIState) -> Widget ()
  cs s c = runReader (campaignStatus c) s

  se _ (AppEvent (CampaignUpdated c')) = continue (c', Running)
  se _ (AppEvent (CampaignTimedout c')) = continue (c', Timedout)
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
ui v w ts d = do
  let d' = fromMaybe defaultDict d
  let getSeed = view $ hasLens . to seed . non (d' ^. defSeed)
  bc <- liftIO $ newBChan 100
  ref <- liftIO $ newIORef defaultCampaign
  dash <- liftM2 (&&) isTerminal $ view (hasLens . dashboard)
  let updateRef = use hasLens >>= liftIO . atomicWriteIORef ref
  let updateUI e = when dash $ readIORef ref >>= writeBChan bc . e
  waitForMe <- liftIO newEmptyMVar
  ticker <- liftIO $ forkIO -- update UI every 100ms, instant exit when no UI
    (when dash $ forever $ threadDelay 100000 >> updateUI CampaignUpdated)
  timeoutSeconds <- (* 1000000) . fromMaybe (-1) <$> view (hasLens . maxTime)
  _ <- forkFinally -- run worker
    (void $ timeout timeoutSeconds (campaign updateRef v w ts d) >>= \case
       Nothing -> liftIO $ updateUI CampaignTimedout
       Just _ -> liftIO $ updateUI CampaignUpdated
    )
    (const $ liftIO $ killThread ticker >> putMVar waitForMe ())
  app <- customMain (mkVty defaultConfig) (Just bc) <$> monitor
  liftIO $ if dash
             then void $ app (defaultCampaign, Uninitialized)
             else takeMVar waitForMe
  final <- liftIO $ readIORef ref
  liftIO . putStrLn =<< view (hasLens . finished) <*> pure final <*> getSeed
  return final
