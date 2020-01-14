{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.UI where

import Brick
import Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import Control.Monad (liftM2, liftM3, void, when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader)
import Control.Monad.Random.Strict (MonadRandom)
import Data.Either (either)
import Data.Has (Has(..))
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (catMaybes, maybe, fromMaybe)
import Data.Set (Set)
import EVM (VM)
import EVM.Types (Addr, W256)
import Graphics.Vty (Event(..), Key(..), Modifier(..), defaultConfig, mkVty)
import System.Posix.Terminal (queryTerminal)
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (killThread, forkIO)

import qualified Data.Text as T

import Echidna.Campaign
import Echidna.Defaults (defaultCampaign, defaultDict)
import Echidna.Exec
import Echidna.Pretty (ppSolCall)
import Echidna.Solidity
import Echidna.Solidity.Types (SolTest)
import Echidna.Test
import Echidna.Transaction
import Echidna.Types (TestState(..), Campaign(..), CampaignConf(..), GenDict, Tx(Tx), defSeed, coverage, src)

data UIConf = UIConf { _dashboard :: Bool
                     , _maxTime   :: Maybe Int
                     , _finished  :: Campaign -> Int -> String
                     }

makeLenses ''UIConf

data UIState = Uninitialized | Running | Timedout

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Given rules for pretty-printing associated address, and whether to print them, pretty-print a 'Transaction'.
ppTx :: (MonadReader x m, Has Names x, Has TxConf x) => Bool -> Tx -> m String
ppTx pn (Tx c s r g gp v (t, b)) = let sOf = either ppSolCall (const "<CREATE>") in do
  names <- view hasLens
  tGas  <- view $ hasLens . txGas
  return $ sOf c ++ (if not pn    then "" else names Sender s ++ names Receiver r)
                 ++ (if g == tGas then "" else " Gas: "         ++ show g)
                 ++ (if gp == 0   then "" else " Gas price: "   ++ show gp)
                 ++ (if v == 0    then "" else " Value: "       ++ show v)
                 ++ (if t == 0    then "" else " Time delay: "  ++ show t)
                 ++ (if b == 0    then "" else " Block delay: " ++ show b)

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" ++ show n ++ "/" ++ show m ++ ")"

-- | Pretty-print the status of a solved test.
ppFail :: (MonadReader x m, Has Names x, Has TxConf x) => Maybe (Int, Int) -> [Tx] -> m String
ppFail _ [] = pure "failed with no transactions made ⁉️  "
ppFail b xs = let status = case b of
                                Nothing    -> ""
                                Just (n,m) -> ", shrinking " ++ progress n m
                  pxs = mapM (ppTx $ length (nub $ view src <$> xs) /= 1) xs in
 (("failed!💥  \n  Call sequence" ++ status ++ ":\n") ++) . unlines . fmap ("    " ++) <$> pxs

-- | Pretty-print the status of a test.
ppTS :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => TestState -> m String
ppTS (Failed e)  = pure $ "could not evaluate ☣\n  " ++ show e
ppTS (Solved l)  = ppFail Nothing l
ppTS Passed      = pure "passed! 🎉"
ppTS (Open i)    = view hasLens >>= \(CampaignConf t _ _ _ _ _ _) ->
                     if i >= t then ppTS Passed else pure $ "fuzzing " ++ progress i t
ppTS (Large n l) = view (hasLens . to shrinkLimit) >>= \m -> ppFail (if n < m then Just (n,m) 
                                                                              else Nothing) l

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppTests (Campaign ts _ _) = unlines . catMaybes <$> mapM pp ts where
  pp (Left  (n, _), s)      = Just .                    ((T.unpack n ++ ": ") ++) <$> ppTS s
  pp (Right _,      Open _) = pure Nothing
  pp (Right (n, _), s)      = Just . (("assertion in " ++ T.unpack n ++ ": ") ++) <$> ppTS s

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: Map W256 (Set Int) -> Maybe String
ppCoverage s | s == mempty = Nothing 
             | otherwise   = Just $ "Unique instructions: " ++ show (coveragePoints s)
                                 ++ "\nUnique codehashes: " ++ show (length s)

ppCampaign :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppCampaign c = (++) <$> ppTests c <*> pure (maybe "" ("\n" ++) . ppCoverage $ c ^. coverage)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
               => (Campaign, UIState) -> m (Widget ())
campaignStatus (c, s) = do
  let mSection = flip (<=>) . maybe emptyWidget ((hBorder <=>) . padLeft (Pad 2))
      mainbox = hCenter . hLimit 120 . joinBorders . borderWithLabel (str "Echidna") . padLeft (Pad 2)
  status <- mainbox . mSection (fmap str . ppCoverage $ c ^. coverage) . str <$> ppTests c
  let bl msg = status <=> hCenter (str msg)
  (s,) <$> isDone c <&> \case
    (Uninitialized, _) -> mainbox $ str "Starting up, please wait  "
    (Timedout, _)      -> bl "Timed out, C-c or esc to print report"
    (_, True)          -> bl "Campaign complete, C-c or esc to print report"
    _                  -> status

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
        => m (App (Campaign, UIState) Campaign ())
monitor = let
  cs :: (CampaignConf, Names, TxConf) -> (Campaign, UIState) -> Widget ()
  cs s c = runReader (campaignStatus c) s

  se _ (AppEvent c') = continue (c', Running)
  se c (VtyEvent (EvKey KEsc _))                         = halt c
  se c (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = halt c
  se c _                                                 = continue c in
    liftM3 (,,) (view hasLens) (view hasLens) (view hasLens) <&> \s ->
      App (pure . cs s) neverShowCursor se pure (const $ forceAttrMap mempty)

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
