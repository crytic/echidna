{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.UI.Report where

import Control.Lens
import Control.Monad.Reader (MonadReader)
import Data.Has (Has(..))
import Data.List (intercalate, nub, sortOn)
import Data.Map (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import EVM.Types (Addr)

import qualified Data.Text as T

import Echidna.ABI (defSeed)
import Echidna.Exec
import Echidna.Pretty (ppTxCall)
import Echidna.Types.Campaign
import Echidna.Types.Tx (Tx(Tx), TxCall(..), TxConf, txGas, src)

-- | An address involved with a 'Transaction' is either the sender, the recipient, or neither of those things.
data Role = Sender | Receiver | Ambiguous

-- | Rules for pretty-printing addresses based on their role in a transaction.
type Names = Role -> Addr -> String

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" ++ show n ++ "/" ++ show m ++ ")"

-- | Given rules for pretty-printing associated address, and whether to print them, pretty-print a 'Transaction'.
ppTx :: (MonadReader x m, Has Names x, Has TxConf x) => Bool -> Tx -> m String
ppTx _ (Tx NoCall _ _ _ _ _ (t, b)) =
  return $ "*wait*" ++ (if t == 0    then "" else " Time delay: "  ++ show (toInteger t) ++ " seconds")
                    ++ (if b == 0    then "" else " Block delay: " ++ show (toInteger b))

ppTx pn (Tx c s r g gp v (t, b)) = let sOf = ppTxCall in do
  names <- view hasLens
  tGas  <- view $ hasLens . txGas
  return $ sOf c ++ (if not pn    then "" else names Sender s ++ names Receiver r)
                 ++ (if g == tGas then "" else " Gas: "         ++ show g)
                 ++ (if gp == 0   then "" else " Gas price: "   ++ show gp)
                 ++ (if v == 0    then "" else " Value: "       ++ show v)
                 ++ (if t == 0    then "" else " Time delay: "  ++ show (toInteger t) ++ " seconds")
                 ++ (if b == 0    then "" else " Block delay: " ++ show (toInteger b))

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: CoverageMap -> Maybe String
ppCoverage s | s == mempty = Nothing
             | otherwise   = Just $ "Unique instructions: " ++ show (scoveragePoints s)
                                 ++ "\nUnique codehashes: " ++ show (length s)

-- | Pretty-print the gas usage for a function.
ppGasOne :: (MonadReader x m, Has Names x, Has TxConf x) => (Text, (Int, [Tx])) -> m String
ppGasOne ("", _)      = pure ""
ppGasOne (f, (g, xs)) = let pxs = mapM (ppTx $ length (nub $ view src <$> xs) /= 1) xs in
 (("\n" ++ unpack f ++ " used a maximum of " ++ show g ++ " gas\n  Call sequence:\n") ++) . unlines . fmap ("    " ++) <$> pxs

-- | Pretty-print the gas usage information a 'Campaign' has obtained.
ppGasInfo :: (MonadReader x m, Has Names x, Has TxConf x) => Campaign -> m String
ppGasInfo Campaign { _gasInfo = gi } | gi == mempty = pure ""
ppGasInfo Campaign { _gasInfo = gi } = (fmap $ intercalate "") (mapM ppGasOne $ sortOn (\(_, (n, _)) -> n) $ toList gi)

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
ppTS (Open i)    = do
  t <- view (hasLens .  testLimit)
  if i >= t then ppTS Passed else pure $ "fuzzing " ++ progress i t
ppTS (Large n l) = do
  m <- view (hasLens . shrinkLimit)
  ppFail (if n < m then Just (n, m) else Nothing) l

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppTests Campaign { _tests = ts } = unlines . catMaybes <$> mapM pp ts where
  pp (Left  (n, _), s)      = Just .                    ((T.unpack n ++ ": ") ++) <$> ppTS s
  pp (Right _,      Open _) = pure Nothing
  pp (Right (n, _), s)      = Just . (("assertion in " ++ T.unpack n ++ ": ") ++) <$> ppTS s

ppCampaign :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppCampaign c = do
  testsPrinted <- ppTests c
  gasInfoPrinted <- ppGasInfo c
  let coveragePrinted = maybe "" ("\n" ++) . ppCoverage $ c ^. coverage
      seedPrinted = "\nSeed: " ++ show (c ^. genDict . defSeed)
  pure $
    testsPrinted
    ++ gasInfoPrinted
    ++ coveragePrinted
    ++ seedPrinted
