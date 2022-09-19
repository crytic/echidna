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
import Control.Lens
import Control.Monad (forever, void, when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader)
import Control.Monad.Random.Strict (MonadRandom)
import qualified Data.ByteString.Lazy as BS
import Data.Has (Has(..))
import Data.Maybe (fromMaybe)
import Data.IORef
import EVM (VM)
import Graphics.Vty (Config, Event(..), Key(..), Modifier(..), defaultConfig, inputMap, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO, forkFinally)
import UnliftIO.Timeout (timeout)

import Echidna.Campaign (campaign)
import Echidna.ABI
import qualified Echidna.Output.JSON
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Campaign
import Echidna.Types.Test (TestConf(..), EchidnaTest)
import Echidna.Types.Tx (Tx, TxConf)
import Echidna.Types.World (World)
import Echidna.UI.Report
import Echidna.UI.Widgets

import EVM.Dapp (DappInfo)

data UIConf = UIConf { _maxTime       :: Maybe Int
                     , _operationMode :: OperationMode
                     }
data OperationMode = Interactive | NonInteractive OutputFormat deriving Show
data OutputFormat = Text | JSON | None deriving Show

instance Read OutputFormat where
  readsPrec _ = \case 't':'e':'x':'t':r -> [(Text, r)]
                      'j':'s':'o':'n':r -> [(JSON, r)]
                      'n':'o':'n':'e':r -> [(None, r)]
                      _ -> []

makeLenses ''UIConf

data CampaignEvent = CampaignUpdated Campaign | CampaignTimedout Campaign

-- | Set up and run an Echidna 'Campaign' and display interactive UI or
-- print non-interactive output in desired format at the end
ui :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadUnliftIO m
      , Has SolConf x, Has TestConf x, Has TxConf x, Has CampaignConf x
      , Has Names x, Has TxConf x, Has UIConf x, Has DappInfo x)
   => VM             -- ^ Initial VM state
   -> World          -- ^ Initial world state
   -> [EchidnaTest]  -- ^ Tests to evaluate
   -> Maybe GenDict
   -> [[Tx]]
   -> m Campaign
ui vm world ts d txs = do
  campaignConf <- view hasLens
  ref <- liftIO $ newIORef defaultCampaign
  let updateRef = use hasLens >>= liftIO . atomicWriteIORef ref
      secToUsec = (* 1000000)
      timeoutUsec = secToUsec $ fromMaybe (-1) (campaignConf ^. maxTime)
      runCampaign = timeout timeoutUsec (campaign updateRef vm world ts d txs)
  terminalPresent <- isTerminal
  let effectiveMode = case campaignConf ^. operationMode of
        Interactive | not terminalPresent -> NonInteractive Text
        other -> other
  case effectiveMode of
    Interactive -> do
      bc <- liftIO $ newBChan 100
      let updateUI e = readIORef ref >>= writeBChan bc . e
      ticker <- liftIO $ forkIO $ -- run UI update every 100ms
        forever $ threadDelay 100000 >> updateUI CampaignUpdated
      _ <- forkFinally -- run worker
        (void $ runCampaign >>= \case
          Nothing -> liftIO $ updateUI CampaignTimedout
          Just _ -> liftIO $ updateUI CampaignUpdated
        )
        (const $ liftIO $ killThread ticker)
      let vty = mkVty vtyConfig
      initialVty <- liftIO vty
      app <- customMain initialVty vty (Just bc) <$> monitor
      liftIO $ void $ app (defaultCampaign, Uninitialized)
      final <- liftIO $ readIORef ref
      liftIO . putStrLn =<< ppCampaign final
      pure final

    NonInteractive outputFormat -> do
      result <- runCampaign
      (final, timedout) <- case result of
        Nothing -> do
          final <- liftIO $ readIORef ref
          pure (final, True)
        Just final ->
          pure (final, False)
      case outputFormat of
        JSON ->
          liftIO . BS.putStr $ Echidna.Output.JSON.encodeCampaign final
        Text -> do
          liftIO . putStrLn =<< ppCampaign final
          when timedout $ liftIO $ putStrLn "TIMEOUT!"
        None ->
          pure ()
      pure final

vtyConfig :: Config
vtyConfig = defaultConfig { inputMap = (Nothing, "\ESC[6;2~", EvKey KPageDown [MShift]) :
                                       (Nothing, "\ESC[5;2~", EvKey KPageUp [MShift]) :
                                       inputMap defaultConfig
                          }

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
        => m (App (Campaign, UIState) CampaignEvent ())
monitor = do
  let cs :: (CampaignConf, Names, TxConf) -> (Campaign, UIState) -> Widget ()
      cs s c = runReader (campaignStatus c) s

      se _ (AppEvent (CampaignUpdated c')) = continue (c', Running)
      se _ (AppEvent (CampaignTimedout c')) = continue (c', Timedout)
      se c (VtyEvent (EvKey KEsc _))                         = halt c
      se c (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = halt c
      se c _                                                 = continue c
  s <- (,,) <$> view hasLens <*> view hasLens <*> view hasLens
  pure $ App (pure . cs s) neverShowCursor se pure (const attrs)

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: MonadIO m => m Bool
isTerminal = liftIO $ (&&) <$> queryTerminal (Fd 0) <*> queryTerminal (Fd 1)
