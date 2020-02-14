{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.UI.Report where

import Control.Lens
import Control.Monad.Reader (MonadReader, liftM, liftM2)
import Data.Has (Has(..))
import Data.List (intercalate, nub)
import Data.Map (Map, toList)
import Data.Maybe (catMaybes, maybe)
import Data.Set (Set)
import Data.Text (Text, unpack)
import EVM.Types (Addr, W256)

import qualified Data.Text as T

import Echidna.Campaign
import Echidna.Exec
import Echidna.Transaction

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" ++ show n ++ "/" ++ show m ++ ")"

-- | Given rules for pretty-printing associated address, and whether to print them, pretty-print a 'Transaction'.
ppTx :: (MonadReader x m, Has Names x, Has TxConf x) => Bool -> Tx -> m String
ppTx pn (Tx c s r g gp v (t, b)) = let sOf = ppTxCall in do
  names <- view hasLens
  tGas  <- view $ hasLens . txGas
  return $ sOf c ++ (if not pn    then "" else names Sender s ++ names Receiver r)
                 ++ (if g == tGas then "" else " Gas: "         ++ show g)
                 ++ (if gp == 0   then "" else " Gas price: "   ++ show gp)
                 ++ (if v == 0    then "" else " Value: "       ++ show v)
                 ++ (if t == 0    then "" else " Time delay: "  ++ show t)
                 ++ (if b == 0    then "" else " Block delay: " ++ show b)

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: Map W256 (Set Int) -> Maybe String
ppCoverage s | s == mempty = Nothing
             | otherwise   = Just $ "Unique instructions: " ++ show (coveragePoints s)
                                 ++ "\nUnique codehashes: " ++ show (length s)

-- | Pretty-print the gas usage for a function.
ppGasOne :: (MonadReader x m, Has Names x, Has TxConf x) => (Text, (Int, [Tx])) -> m String
ppGasOne ("", _)      = pure ""
ppGasOne (f, (g, xs)) = let pxs = mapM (ppTx $ length (nub $ view src <$> xs) /= 1) xs in
 (("\n" ++ unpack f ++ " used a maximum of " ++ show g ++ " gas\n  Call sequence:\n") ++) . unlines . fmap ("    " ++) <$> pxs

-- | Pretty-print the gas usage information a 'Campaign' has obtained.
ppGasInfo :: (MonadReader x m, Has Names x, Has TxConf x) => Campaign -> m String
ppGasInfo (Campaign _ _ gi _) | gi == mempty = pure ""
ppGasInfo (Campaign _ _ gi _) = (liftM $ intercalate "") (mapM ppGasOne $ toList gi)

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
ppTS (Open i)    = view hasLens >>= \(CampaignConf t _ _ _ _ _ _ _) ->
                     if i >= t then ppTS Passed else pure $ "fuzzing " ++ progress i t
ppTS (Large n l) = view (hasLens . to shrinkLimit) >>= \m -> ppFail (if n < m then Just (n,m)
                                                                              else Nothing) l

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppTests (Campaign ts _ _ _) = unlines . catMaybes <$> mapM pp ts where
  pp (Left  (n, _), s)      = Just .                    ((T.unpack n ++ ": ") ++) <$> ppTS s
  pp (Right _,      Open _) = pure Nothing
  pp (Right (n, _), s)      = Just . (("assertion in " ++ T.unpack n ++ ": ") ++) <$> ppTS s

ppCampaign :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppCampaign c = (++) <$> (liftM2 (++) (ppTests c) (ppGasInfo c)) <*> pure (maybe "" ("\n" ++) . ppCoverage $ c ^. coverage)


