{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad (forever, liftM3)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, runReader)
import Control.Monad.Random.Strict (MonadRandom)
import Data.Bool (bool)
import Data.Either (either)
import Data.Has (Has(..))
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (maybe, fromMaybe)
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
import Echidna.Solidity
import Echidna.Test
import Echidna.Transaction

data UIConf = UIConf { _dashboard :: Bool
                     , _finished  :: Campaign -> Int -> String
                     }

makeLenses ''UIConf

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Given rules for pretty-printing associated address, and whether to print them, pretty-print a 'Transaction'.
ppTx :: (MonadReader x m, Has Names x, Has TxConf x) => Bool -> Tx -> m String
ppTx b (Tx c s r g v) = let sOf = either ppSolCall (const "<CREATE>") in do
  names <- view hasLens
  tGas  <- view $ hasLens . txGas
  return $ sOf c ++ (if not b     then "" else names Sender s ++ names Receiver r)
                 ++ (if g == tGas then "" else " Gas: "   ++ show g)
                 ++ (if v == 0    then "" else " Value: " ++ show v)

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
ppTS (Open i)    = view hasLens >>= \(CampaignConf t _ _ _ _) ->
                     if i >= t then ppTS Passed else pure $ "fuzzing " ++ progress i t
ppTS (Large n l) = view (hasLens . to shrinkLimit) >>= \m -> ppFail (if n < m then Just (n,m) 
                                                                              else Nothing) l

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppTests (Campaign ts _ _) = unlines <$> mapM pp ts where
  pp (Left _,       Open _) = pure ""
  pp (Left  (n, _), s)      =                    ((T.unpack n ++ ": ") ++) <$> ppTS s
  pp (Right (n, _), s)      = (("assertion in " ++ T.unpack n ++ ": ") ++) <$> ppTS s

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: Map W256 (Set Int) -> Maybe String
ppCoverage s | s == mempty = Nothing 
             | otherwise   = Just $ "Unique instructions: " ++ show (coveragePoints s)
                                 ++ "\nUnique codehashes: " ++ show (length s)

ppCampaign :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppCampaign c = (++) <$> ppTests c <*> pure (maybe "" ("\n" ++) . ppCoverage $ c ^. coverage)

-- | Render 'Campaign' progress as a 'Widget'.
campaignStatus :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
               => Campaign -> m (Widget ())
campaignStatus c = let mSection = maybe emptyWidget ((hBorder <=>) . padLeft (Pad 2) . str) in do
  stats <- padLeft (Pad 2) . str <$> ppTests c <&> (<=> mSection (ppCoverage $ c ^. coverage))
  bl <- bool emptyWidget (str "Campaign complete, C-c or esc to print report") <$> isDone c
  pure . hCenter . hLimit 120 . joinBorders $ borderWithLabel (str "Echidna") stats <=> bl

-- | Check if we should stop drawing (or updating) the dashboard, then do the right thing.
monitor :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x)
        => IO a -> m (App Campaign Campaign ())
monitor cleanup = let
  cs :: (CampaignConf, Names, TxConf) -> Campaign -> Widget ()
  cs s c = runReader (campaignStatus c) s

  se s _ (AppEvent c') = continue c' & if runReader (isDone c') s then (liftIO cleanup >>) else id
  se _ c (VtyEvent (EvKey KEsc _))                         = liftIO cleanup >> halt c
  se _ c (VtyEvent (EvKey (KChar 'c') l)) | MCtrl `elem` l = liftIO cleanup >> halt c
  se _ c _                                                 = continue c in
    liftM3 (,,) (view hasLens) (view hasLens) (view hasLens) <&> \s ->
      App (pure . cs s) neverShowCursor (se s) pure (const $ forceAttrMap mempty)

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
ui v w ts d = let xfer e = use hasLens >>= \c -> isDone c >>= ($ e c) . bool id forever in do
  let d' = fromMaybe defaultDict d
  s <- (&&) <$> isTerminal <*> view (hasLens . dashboard)
  g <- view (hasLens . to seed)
  let g' = fromMaybe (d' ^. defSeed) g
  c <- if s then do bc <- liftIO $ newBChan 100
                    t <- forkIO $ campaign (xfer $ liftIO . writeBChan bc) v w ts d >> pure ()
                    a <- monitor (killThread t)
                    liftIO (customMain (mkVty defaultConfig) (Just bc) a $ Campaign mempty mempty d')
            else campaign (pure ()) v w ts d
  liftIO . putStrLn =<< view (hasLens . finished) <*> pure c <*> pure g'
  return c
