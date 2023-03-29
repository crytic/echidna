module Echidna.UI.Report where

import Control.Monad.Reader (MonadReader, asks, MonadIO (liftIO))
import Data.List (intercalate, nub, sortOn)
import Data.Map (toList)
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Text qualified as T

import Echidna.ABI (GenDict(..), encodeSig)
import Echidna.Events (Events)
import Echidna.Pretty (ppTxCall)
import Echidna.Types (Gas)
import Echidna.Types.Campaign
import Echidna.Types.Corpus (Corpus, corpusSize)
import Echidna.Types.Coverage (CoverageMap, FrozenCoverageMap, scoveragePoints, scoveragePointsFrozen)
import Echidna.Types.Test (EchidnaTest(..), TestState(..), TestType(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))
import Echidna.Types.Config

import EVM.Types (W256)

ppCampaign :: (MonadIO m, MonadReader Env m) => Campaign -> m String
ppCampaign campaign = do
  testsPrinted <- ppTests campaign
  gasInfoPrinted <- ppGasInfo campaign
  coveragePrinted <- liftIO $ ppCoverage campaign.coverage
  let corpusPrinted = "\n" <> ppCorpus campaign.corpus
      seedPrinted = "\nSeed: " <> show campaign.genDict.defSeed
  pure $
    testsPrinted
    <> gasInfoPrinted
    <> coveragePrinted
    <> corpusPrinted
    <> seedPrinted

-- | Given rules for pretty-printing associated address, and whether to print them, pretty-print a 'Transaction'.
ppTx :: MonadReader Env m => Bool -> Tx -> m String
ppTx _ Tx { call = NoCall, delay } =
  pure $ "*wait*" <> ppDelay delay
ppTx printName tx = do
  names <- asks (.cfg.namesConf)
  tGas  <- asks (.cfg.txConf.txGas)
  pure $
    ppTxCall tx.call
    <> (if not printName then "" else names Sender tx.src <> names Receiver tx.dst)
    <> (if tx.gas == tGas then "" else " Gas: " <> show tx.gas)
    <> (if tx.gasprice == 0 then "" else " Gas price: " <> show tx.gasprice)
    <> (if tx.value == 0 then "" else " Value: " <> show tx.value)
    <> ppDelay tx.delay

ppDelay :: (W256, W256) -> [Char]
ppDelay (time, block) =
  (if time == 0 then "" else " Time delay: " <> show (toInteger time) <> " seconds")
  <> (if block == 0 then "" else " Block delay: " <> show (toInteger block))

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: CoverageMap -> IO String
ppCoverage s = do
  points <- scoveragePoints s
  pure $ ppCoverageCommon points (length s)

ppFrozenCoverage :: FrozenCoverageMap -> String
ppFrozenCoverage s = ppCoverageCommon (scoveragePointsFrozen s) (length s)

ppCoverageCommon :: Int -> Int -> String
ppCoverageCommon points ncodehashes =
  "Unique instructions: " <> show points
    <> "\nUnique codehashes: " <> show ncodehashes

-- | Pretty-print the corpus a 'Campaign' has obtained.
ppCorpus :: Corpus -> String
ppCorpus c = "Corpus size: " <> show (corpusSize c)

-- | Pretty-print the gas usage information a 'Campaign' has obtained.
ppGasInfo :: MonadReader Env m => Campaign -> m String
ppGasInfo Campaign { gasInfo } | gasInfo == mempty = pure ""
ppGasInfo Campaign { gasInfo } = do
  items <- mapM ppGasOne $ sortOn (\(_, (n, _)) -> n) $ toList gasInfo
  pure $ intercalate "" items

-- | Pretty-print the gas usage for a function.
ppGasOne :: MonadReader Env m => (Text, (Gas, [Tx])) -> m String
ppGasOne ("", _)      = pure ""
ppGasOne (func, (gas, txs)) = do
  let header = "\n" <> unpack func <> " used a maximum of " <> show gas <> " gas\n"
               <> "  Call sequence:\n"
  prettyTxs <- mapM (ppTx $ length (nub $ (.src) <$> txs) /= 1) txs
  pure $ header <> unlines (("    " <>) <$> prettyTxs)

-- | Pretty-print the status of a solved test.
ppFail :: MonadReader Env m => Maybe (Int, Int) -> Events -> [Tx] -> m String
ppFail _ _ []  = pure "failed with no transactions made ⁉️  "
ppFail b es xs = do
  let status = case b of
        Nothing    -> ""
        Just (n,m) -> ", shrinking " <> progress n m
  prettyTxs <- mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs
  pure $ "failed!💥  \n  Call sequence" <> status <> ":\n"
         <> unlines (("    " <>) <$> prettyTxs) <> "\n"
         <> ppEvents es

ppEvents :: Events -> String
ppEvents es = if null es then "" else "Event sequence: " <> T.unpack (T.intercalate ", " es)

-- | Pretty-print the status of a test.

ppTS :: MonadReader Env m => TestState -> Events -> [Tx] -> m String
ppTS (Failed e) _ _  = pure $ "could not evaluate ☣\n  " <> show e
ppTS Solved     es l = ppFail Nothing es l
ppTS Passed     _ _  = pure " passed! 🎉"
ppTS (Open i)   es [] = do
  t <- asks (.cfg.campaignConf.testLimit)
  if i >= t then ppTS Passed es [] else pure $ " fuzzing " <> progress i t
ppTS (Open _)   es r = ppFail Nothing es r
ppTS (Large n) es l  = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  ppFail (if n < m then Just (n, m) else Nothing) es l

ppOPT :: MonadReader Env m => TestState -> Events -> [Tx] -> m String
ppOPT (Failed e) _ _  = pure $ "could not evaluate ☣\n  " <> show e
ppOPT Solved     es l = ppOptimized Nothing es l
ppOPT Passed     _ _  = pure " passed! 🎉"
ppOPT (Open _)   es r = ppOptimized Nothing es r
ppOPT (Large n) es l  = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  ppOptimized (if n < m then Just (n, m) else Nothing) es l

-- | Pretty-print the status of a optimized test.
ppOptimized :: MonadReader Env m => Maybe (Int, Int) -> Events -> [Tx] -> m String
ppOptimized _ _ []  = pure "Call sequence:\n(no transactions)"
ppOptimized b es xs = do
  let status = case b of
        Nothing    -> ""
        Just (n,m) -> ", shrinking " <> progress n m
  prettyTxs <- mapM (ppTx $ length (nub $ (.src) <$> xs) /= 1) xs
  pure $ "\n  Call sequence" <> status <> ":\n"
         <> unlines (("    " <>) <$> prettyTxs) <> "\n"
         <> ppEvents es

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: MonadReader Env m => Campaign -> m String
ppTests Campaign { tests } = unlines . catMaybes <$> mapM pp tests
  where
  pp t =
    case t.testType of
      PropertyTest n _ -> do
        status <- ppTS t.state t.events t.reproducer
        pure $ Just (T.unpack n <> ": " <> status)
      CallTest n _ -> do
        status <- ppTS t.state t.events t.reproducer
        pure $ Just (T.unpack n <> ": " <> status)
      AssertionTest _ s _ -> do
        status <- ppTS t.state t.events t.reproducer
        pure $ Just (T.unpack (encodeSig s) <> ": " <> status)
      OptimizationTest n _ -> do
        status <- ppOPT t.state t.events t.reproducer
        pure $ Just (T.unpack n <> ": max value: " <> show t.value <> "\n" <> status)
      Exploration -> pure Nothing

-- | Given a number of boxes checked and a number of total boxes, pretty-print progress in box-checking.
progress :: Int -> Int -> String
progress n m = "(" <> show n <> "/" <> show m <> ")"
