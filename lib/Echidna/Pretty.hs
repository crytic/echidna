{-# LANGUAGE FlexibleContexts #-}

module Echidna.Pretty where

import Control.Lens           (view, to, (^.))
import Control.Monad.Reader   (MonadReader)
import Data.Foldable          (toList)
import Data.Has               (Has(..))
import Data.List              (intercalate, nub)
import Data.Map               (Map)
import Data.Maybe             (catMaybes)
import Data.Set               (Set)
import Data.Text              (unpack)
import Echidna.Exec           (coveragePoints)
import Echidna.Solidity.Types (SolCall)
import Echidna.Transaction    (TxConf, txGas)
import Echidna.Types          (Campaign(..), CampaignConf(..), TestState(..), Tx(..), Role(..), Names, src, coverage)
import EVM.ABI                (AbiValue(..))
import EVM.Types              (W256)
import Numeric                (showHex)

-- | Pretty-print some 'AbiValue'.
ppAbiValue :: AbiValue -> String
ppAbiValue (AbiUInt _ n)         = show n
ppAbiValue (AbiInt  _ n)         = show n
ppAbiValue (AbiAddress n)        = showHex n ""
ppAbiValue (AbiBool b)           = if b then "true" else "false"
ppAbiValue (AbiBytes      _ b)   = show b
ppAbiValue (AbiBytesDynamic b)   = show b
ppAbiValue (AbiString       s)   = show s
ppAbiValue (AbiArrayDynamic _ v) =
  "[" ++ intercalate ", " (ppAbiValue <$> toList v) ++ "]"
ppAbiValue (AbiArray      _ _ v) =
  "[" ++ intercalate ", " (ppAbiValue <$> toList v) ++ "]"
ppAbiValue (AbiTuple v) =
  "(" ++ intercalate ", " (ppAbiValue <$> toList v) ++ ")"

-- | Pretty-print some 'SolCall'.
ppSolCall :: SolCall -> String
ppSolCall (t, vs) = (if t == "" then unpack "*fallback*" else unpack t) ++ "(" ++ intercalate "," (ppAbiValue <$> vs) ++ ")"

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" ++ show n ++ "/" ++ show m ++ ")"

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
  pp (Left  (n, _), s)      = Just .                    ((unpack n ++ ": ") ++) <$> ppTS s
  pp (Right _,      Open _) = pure Nothing
  pp (Right (n, _), s)      = Just . (("assertion in " ++ unpack n ++ ": ") ++) <$> ppTS s

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: Map W256 (Set Int) -> Maybe String
ppCoverage s | s == mempty = Nothing 
             | otherwise   = Just $ "Unique instructions: " ++ show (coveragePoints s)
                                 ++ "\nUnique codehashes: " ++ show (length s)

ppCampaign :: (MonadReader x m, Has CampaignConf x, Has Names x, Has TxConf x) => Campaign -> m String
ppCampaign c = (++) <$> ppTests c <*> pure (maybe "" ("\n" ++) . ppCoverage $ c ^. coverage)
