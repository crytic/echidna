{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import Control.Monad (forever, liftM2)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Random.Strict (MonadRandom)
import Control.Monad.Reader (MonadReader, runReader)
import Data.Bool (bool)
import Data.Either (either)
import Data.Has (Has(..))
import Data.Map (Map)
import Data.Maybe (maybe)
import Data.Set (Set)
import EVM (VM)
import EVM.Types (Addr, W256)
import Graphics.Vty (Event(..), Key(..), Modifier(..), defaultConfig, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO, killThread)

import qualified Data.Text as T

import Echidna.Campaign
import Echidna.ABI
import Echidna.Exec
import Echidna.Test
import Echidna.Transaction

data UIConf = UIConf { _dashboard :: Bool
                     , _finished  :: Campaign -> String
                     }

makeLenses ''UIConf

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Given rules for pretty-printing associated address, pretty-print a 'Transaction'.
ppTx :: (MonadReader x m, Has Names x) => Tx -> m String
ppTx (Tx c s r v) = let sOf = either ppSolCall (const "<CREATE>") in
  view hasLens <&> \f -> sOf c ++ f Sender s ++ f Receiver r
                      ++ (if v == 0 then "" else "Value: " ++ show v)

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" ++ show n ++ "/" ++ show m ++ ")"

-- | Pretty-print the status of a solved test.
ppFail :: (MonadReader x m, Has Names x) => Maybe (Int, Int) -> [Tx] -> m String
ppFail _ [] = pure "failed with no transactions made ⁉️  "
ppFail b xs = let status = case b of
                                Nothing    -> ""
                                Just (n,m) -> ", shrinking " ++ progress n m in
 (("failed!💥  \n  Call sequence" ++ status ++ ":\n") ++) . unlines . fmap ("    " ++) <$> mapM ppTx xs

-- | Pretty-print the status of a test.
ppTS :: (MonadReader x m, Has CampaignConf x, Has Names x) => TestState -> m String
ppTS (Failed e)  = pure $ "could not evaluate ☣\n  " ++ show e
ppTS (Solved l)  = ppFail Nothing l
ppTS Passed      = pure "passed! 🎉"
ppTS (Open i)    = view hasLens >>= \(CampaignConf t _ _ _) ->
                     if i >= t then ppTS Passed else pure $ "fuzzing " ++ progress i t
ppTS (Large n l) = view (hasLens . to shrinkLimit) >>= \m -> ppFail (if n < m then Just (n,m) 
                                                                              else Nothing) l

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: (MonadReader x m, Has CampaignConf x, Has Names x) => Campaign -> m String
ppTests (Campaign ts _ _) = unlines <$> mapM (\((n, _), s) -> ((T.unpack n ++ ": ") ++ ) <$> ppTS s) ts

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: Map W256 (Set Int) -> Maybe String
ppCoverage s | s == mempty = Nothing 
             | otherwise   = Just $ "Unique instructions: " ++ show (coveragePoints s)
                                ++ "\nUnique codehashes: " ++ show (length s)

ppCampaign :: (MonadReader x m, Has CampaignConf x, Has Names x) => Campaign -> m String
ppCampaign c = (++) <$> ppTests c <*> pure (maybe "" ("\n" ++) . ppCoverage $ c ^. coverage)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: (MonadReader x m, Has CampaignConf x, Has Names x) => Campaign -> m (Widget ())
campaignStatus c = let mSection = maybe emptyWidget ((hBorder <=>) . padLeft (Pad 2) . str) in do
  stats <- padLeft (Pad 2) . str <$> ppTests c <&> (<=> mSection (ppCoverage $ c ^. coverage))
  bl <- bool emptyWidget (str "Campaign complete, C-c or esc to print report") <$> isDone c
  pure . hCenter . hLimit 120 $ borderWithLabel (str "Echidna") stats <=> bl

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: (MonadReader x m, Has CampaignConf x, Has Names x)
        => IO a -> m (App Campaign Campaign ())
monitor cleanup = let
  cs :: (CampaignConf, Names) -> Campaign -> Widget ()
  cs s c = runReader (campaignStatus c) s

  se s _ (AppEvent c') = continue c' & if runReader (isDone c') s then (liftIO cleanup >>) else id
  se _ c (VtyEvent (EvKey KEsc _))                         = liftIO cleanup >> halt c
  se _ c (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = liftIO cleanup >> halt c
  se _ c _                                                 = continue c in
    liftM2 (,) (view hasLens) (view hasLens) <&> \s ->
      App (pure . cs s) neverShowCursor (se s) pure (const $ forceAttrMap mempty)

-- | Heuristic check that we're in a sensible terminal (not a pipe)
isTerminal :: MonadIO m => m Bool
isTerminal = liftIO $ queryTerminal (Fd 0)

-- | Set up and run an Echidna 'Campaign' while drawing the dashboard, then print 'Campaign' status
-- once done.
ui :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadUnliftIO m
      , Has TestConf x, Has CampaignConf x, Has Names x, Has UIConf x)
   => VM        -- ^ Initial VM state
   -> World     -- ^ Initial world state
   -> [SolTest] -- ^ Tests to evaluate
   -> Maybe GenDict
   -> m Campaign
ui v w ts d = let xfer e = use hasLens >>= \c -> isDone c >>= ($ e c) . bool id forever in do
  s <- (&&) <$> isTerminal <*> view (hasLens . dashboard)
  c <- if s then do bc <- liftIO $ newBChan 100
                    t <- forkIO $ campaign (xfer $ liftIO . writeBChan bc) v w ts d >> pure ()
                    a <- monitor (killThread t)
                    liftIO (customMain (mkVty defaultConfig) (Just bc) a mempty)
            else campaign (pure ()) v w ts d
  liftIO . putStrLn =<< ($ c) <$> view (hasLens . finished)
  return c
