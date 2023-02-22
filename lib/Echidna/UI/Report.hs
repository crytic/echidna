module Echidna.UI.Report where

import Control.Monad.Reader (MonadReader, asks)
import Data.List (intercalate, nub, sortOn)
import Data.Map (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text qualified as T

import Echidna.ABI (GenDict(..), encodeSig)
import Echidna.Events (Events)
import Echidna.Pretty (ppTxCall)
import Echidna.Types.Campaign
import Echidna.Types.Corpus (Corpus, corpusSize)
import Echidna.Types.Coverage (CoverageMap, scoveragePoints)
import Echidna.Types.Test (testEvents, testState, TestState(..), testType, TestType(..), testReproducer, testValue)
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))
import Echidna.Types.Config

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" ++ show n ++ "/" ++ show m ++ ")"

-- | Given rules for pretty-printing associated address, and whether to print them, pretty-print a 'Transaction'.
ppTx :: (MonadReader EConfig m) => Bool -> Tx -> m String
ppTx _ (Tx NoCall _ _ _ _ _ (t, b)) =
  return $ "*wait*" ++ (if t == 0    then "" else " Time delay: "  ++ show (toInteger t) ++ " seconds")
                    ++ (if b == 0    then "" else " Block delay: " ++ show (toInteger b))

ppTx pn (Tx c s r g gp v (t, b)) = let sOf = ppTxCall in do
  names <- asks (.namesConf)
  tGas  <- asks (.txConf.txGas)
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

-- | Pretty-print the corpus a 'Campaign' has obtained.
ppCorpus :: Corpus -> Maybe String
ppCorpus c | c == mempty = Nothing
           | otherwise   = Just $ "Corpus size: " ++ show (corpusSize c)

-- | Pretty-print the gas usage for a function.
ppGasOne :: MonadReader EConfig m => (Text, (Int, [Tx])) -> m String
ppGasOne ("", _)      = pure ""
ppGasOne (f, (g, xs)) = let pxs = mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs in
 (("\n" ++ unpack f ++ " used a maximum of " ++ show g ++ " gas\n  Call sequence:\n") ++) . unlines . fmap ("    " ++) <$> pxs

-- | Pretty-print the gas usage information a 'Campaign' has obtained.
ppGasInfo :: MonadReader EConfig m => Campaign -> m String
ppGasInfo Campaign { _gasInfo = gi } | gi == mempty = pure ""
ppGasInfo Campaign { _gasInfo = gi } = (fmap $ intercalate "") (mapM ppGasOne $ sortOn (\(_, (n, _)) -> n) $ toList gi)

-- | Pretty-print the status of a solved test.
ppFail :: MonadReader EConfig m => Maybe (Int, Int) -> Events -> [Tx] -> m String
ppFail _ _ []  = pure "failed with no transactions made ⁉️  "
ppFail b es xs = let status = case b of
                                Nothing    -> ""
                                Just (n,m) -> ", shrinking " ++ progress n m
                     pxs = mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs in
 do s <- (("failed!💥  \n  Call sequence" ++ status ++ ":\n") ++) . unlines . fmap ("    " ++) <$> pxs
    return (s ++ "\n" ++ ppEvents es)

ppEvents :: Events -> String
ppEvents es = if null es then "" else "Event sequence: " ++ T.unpack (T.intercalate ", " es)

-- | Pretty-print the status of a test.

ppTS :: MonadReader EConfig m => TestState -> Events -> [Tx] -> m String
ppTS (Failed e) _ _  = pure $ "could not evaluate ☣\n  " ++ show e
ppTS Solved     es l = ppFail Nothing es l
ppTS Passed     _ _  = pure " passed! 🎉"
ppTS (Open i)   es [] = do
  t <- asks (.campaignConf.testLimit)
  if i >= t then ppTS Passed es [] else pure $ " fuzzing " ++ progress i t
ppTS (Open _)   es r = ppFail Nothing es r
ppTS (Large n) es l  = do
  m <- asks (.campaignConf.shrinkLimit)
  ppFail (if n < m then Just (n, m) else Nothing) es l


ppOPT :: MonadReader EConfig m => TestState -> Events -> [Tx] -> m String
ppOPT (Failed e) _ _  = pure $ "could not evaluate ☣\n  " ++ show e
ppOPT Solved     es l = ppOptimized Nothing es l
ppOPT Passed     _ _  = pure " passed! 🎉"
ppOPT (Open _)   es r = ppOptimized Nothing es r
ppOPT (Large n) es l  = do
  m <- asks (.campaignConf.shrinkLimit)
  ppOptimized (if n < m then Just (n, m) else Nothing) es l


-- | Pretty-print the status of a optimized test.
ppOptimized :: MonadReader EConfig m => Maybe (Int, Int) -> Events -> [Tx] -> m String
ppOptimized _ _ []  = pure "Call sequence:\n(no transactions)"
ppOptimized b es xs = let status = case b of
                                Nothing    -> ""
                                Just (n,m) -> ", shrinking " ++ progress n m
                          pxs = mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs in
 do s <- (("\n  Call sequence" ++ status ++ ":\n") ++) . unlines . fmap ("    " ++) <$> pxs
    return (s ++ "\n" ++ ppEvents es)


-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: MonadReader EConfig m => Campaign -> m String
ppTests Campaign { _tests = ts } = unlines . catMaybes <$> mapM pp ts where
  pp t = case t.testType of
         PropertyTest n _      ->  Just . ((T.unpack n ++ ": ") ++) <$> ppTS t.testState t.testEvents t.testReproducer
         CallTest n _          ->  Just . ((T.unpack n ++ ": ") ++) <$> ppTS t.testState t.testEvents t.testReproducer
         AssertionTest _ s _   ->  Just . ((T.unpack (encodeSig s) ++ ": ") ++) <$> ppTS t.testState t.testEvents t.testReproducer
         OptimizationTest n _  ->  Just . ((T.unpack n ++ ": max value: " ++ show t.testValue ++ "\n") ++) <$> ppOPT t.testState t.testEvents t.testReproducer
         Exploration           ->  return Nothing

ppCampaign :: MonadReader EConfig m => Campaign -> m String
ppCampaign c = do
  testsPrinted <- ppTests c
  gasInfoPrinted <- ppGasInfo c
  let coveragePrinted = maybe "" ("\n" ++) . ppCoverage $ c._coverage
      corpusPrinted = maybe "" ("\n" ++) . ppCorpus $ c._corpus
      seedPrinted = "\nSeed: " ++ show c._genDict.defSeed
  pure $
    testsPrinted
    ++ gasInfoPrinted
    ++ coveragePrinted
    ++ corpusPrinted
    ++ seedPrinted
