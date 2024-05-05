module Echidna.UI.Report where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, MonadIO (liftIO), asks)
import Control.Monad.ST (RealWorld)
import Data.IORef (readIORef)
import Data.List (intercalate, nub, sortOn)
import Data.Map (toList)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Time (LocalTime)
import Optics

import Echidna.ABI (GenDict(..), encodeSig)
import Echidna.Pretty (ppTxCall)
import Echidna.SourceMapping (findSrcByMetadata)
import Echidna.Types (Gas)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Corpus (corpusSize)
import Echidna.Types.Coverage (scoveragePoints)
import Echidna.Types.Test (EchidnaTest(..), TestState(..), TestType(..))
import Echidna.Types.Tx (Tx(..), TxCall(..), TxConf(..))
import Echidna.Utility (timePrefix)

import EVM.Format (showTraceTree, contractNamePart)
import EVM.Solidity (SolcContract(..))
import EVM.Types (W256, VM, VMType(Concrete), Addr, Expr (LitAddr))

ppLogLine :: (LocalTime, CampaignEvent) -> String
ppLogLine (time, event@(WorkerEvent workerId FuzzWorker _)) =
  timePrefix time <> "[Worker " <> show workerId <> "] " <> ppCampaignEvent event
ppLogLine (time, event@(WorkerEvent workerId SymbolicWorker _)) =
  timePrefix time <> "[Worker " <> show workerId <> ", symbolic] " <> ppCampaignEvent event
ppLogLine (time, event) =
  timePrefix time <> " " <> ppCampaignEvent event

ppCampaign :: (MonadIO m, MonadReader Env m) => VM Concrete RealWorld -> [WorkerState] -> m String
ppCampaign vm workerStates = do
  tests <- liftIO . readIORef =<< asks (.testsRef)
  testsPrinted <- ppTests tests
  gasInfoPrinted <- ppGasInfo vm workerStates
  coveragePrinted <- ppCoverage
  let seedPrinted = "Seed: " <> show (head workerStates).genDict.defSeed
  corpusPrinted <- ppCorpus
  pure $ unlines
    [ testsPrinted
    , gasInfoPrinted
    , coveragePrinted
    , corpusPrinted
    , seedPrinted
    ]

-- | Given rules for pretty-printing associated address, and whether to print
-- them, pretty-print a 'Transaction'.
ppTx :: MonadReader Env m => VM Concrete RealWorld -> Bool -> Tx -> m String
ppTx _ _ Tx { call = NoCall, delay } =
  pure $ "*wait*" <> ppDelay delay
ppTx vm printName tx = do
  contractName <- case tx.call of
    SolCall _ -> Just <$> contractNameForAddr vm tx.dst
    _ -> pure Nothing
  names <- asks (.cfg.namesConf)
  tGas  <- asks (.cfg.txConf.txGas)
  pure $
    unpack (maybe "" (<> ".") contractName) <> ppTxCall tx.call
    <> (if not printName then "" else names Sender tx.src <> names Receiver tx.dst)
    <> (if tx.gas == tGas then "" else " Gas: " <> show tx.gas)
    <> (if tx.gasprice == 0 then "" else " Gas price: " <> show tx.gasprice)
    <> (if tx.value == 0 then "" else " Value: " <> show tx.value)
    <> ppDelay tx.delay

contractNameForAddr :: MonadReader Env m => VM Concrete RealWorld -> Addr -> m Text
contractNameForAddr vm addr = do
  dapp <- asks (.dapp)
  maybeName <- case Map.lookup (LitAddr addr) (vm ^. #env % #contracts) of
    Just contract ->
      case findSrcByMetadata contract dapp of
        Just solcContract -> pure $ Just $ contractNamePart solcContract.contractName
        Nothing -> pure Nothing
    Nothing -> pure Nothing
  pure $ fromMaybe (T.pack $ show addr) maybeName

ppDelay :: (W256, W256) -> [Char]
ppDelay (time, block) =
  (if time == 0 then "" else " Time delay: " <> show (toInteger time) <> " seconds")
  <> (if block == 0 then "" else " Block delay: " <> show (toInteger block))

-- | Pretty-print the coverage a 'Campaign' has obtained.
ppCoverage :: (MonadIO m, MonadReader Env m) => m String
ppCoverage = do
  coverage <- liftIO . readIORef =<< asks (.coverageRef)
  points <- liftIO $ scoveragePoints coverage
  pure $ "Unique instructions: " <> show points <> "\n" <>
         "Unique codehashes: " <> show (length coverage)

-- | Pretty-print the corpus a 'Campaign' has obtained.
ppCorpus :: (MonadIO m, MonadReader Env m) => m String
ppCorpus = do
  corpus <- liftIO . readIORef =<< asks (.corpusRef)
  pure $ "Corpus size: " <> show (corpusSize corpus)

-- | Pretty-print the gas usage information a 'Campaign' has obtained.
ppGasInfo :: MonadReader Env m => VM Concrete RealWorld -> [WorkerState] -> m String
ppGasInfo vm workerStates = do
  let gasInfo = Map.unionsWith max ((.gasInfo) <$> workerStates)
  items <- mapM (ppGasOne vm) $ sortOn (\(_, (n, _)) -> n) $ toList gasInfo
  pure $ intercalate "" items

-- | Pretty-print the gas usage for a function.
ppGasOne :: MonadReader Env m => VM Concrete RealWorld -> (Text, (Gas, [Tx])) -> m String
ppGasOne _  ("", _)      = pure ""
ppGasOne vm (func, (gas, txs)) = do
  let header = "\n" <> unpack func <> " used a maximum of " <> show gas <> " gas\n"
               <> "  Call sequence:\n"
  prettyTxs <- mapM (ppTx vm $ length (nub $ (.src) <$> txs) /= 1) txs
  pure $ header <> unlines (("    " <>) <$> prettyTxs)

-- | Pretty-print the status of a solved test.
ppFail :: MonadReader Env m => Maybe (Int, Int) -> VM Concrete RealWorld -> [Tx] -> m String
ppFail _ _ []  = pure "failed with no transactions made ⁉️ "
ppFail b vm xs = do
  let status = case b of
        Nothing    -> ""
        Just (n,m) -> ", shrinking " <> progress n m
  prettyTxs <- mapM (ppTx vm $ length (nub $ (.src) <$> xs) /= 1) xs
  dappInfo <- asks (.dapp)
  pure $ "failed!💥  \n  Call sequence" <> status <> ":\n"
         <> unlines (("    " <>) <$> prettyTxs) <> "\n"
         <> "Traces: \n" <> T.unpack (showTraceTree dappInfo vm)

-- | Pretty-print the status of a solved test.
ppFailWithTraces :: MonadReader Env m => Maybe (Int, Int) -> VM Concrete RealWorld -> [(Tx, VM Concrete RealWorld)] -> m String
ppFailWithTraces  _ _ []  = pure "failed with no transactions made ⁉️ "
ppFailWithTraces b finalVM results = do
  dappInfo <- asks (.dapp)
  let xs = fst <$> results
  let status = case b of
        Nothing    -> ""
        Just (n,m) -> ", shrinking " <> progress n m
  let printName = length (nub $ (.src) <$> xs) /= 1
  prettyTxs <- forM results $ \(tx, vm) -> do
    txPrinted <- ppTx vm printName tx
    pure $ txPrinted <> "\nTraces:\n" <> T.unpack (showTraceTree dappInfo vm)
  pure $ "failed!💥  \n  Call sequence" <> status <> ":\n"
         <> unlines (("    " <>) <$> prettyTxs) <> "\n"
         <> "Test traces: \n" <> T.unpack (showTraceTree dappInfo finalVM)

-- | Pretty-print the status of a test.

ppTS :: MonadReader Env m => TestState -> VM Concrete RealWorld -> [Tx] -> m String
ppTS (Failed e) _ _  = pure $ "could not evaluate ☣\n  " <> show e
ppTS Solved     vm l = ppFail Nothing vm l
ppTS Passed     _ _  = pure " passed! 🎉"
ppTS Open      _ []  = pure "passing"
ppTS Open      vm r  = ppFail Nothing vm r
ppTS (Large n) vm l  = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  ppFail (if n < m then Just (n, m) else Nothing) vm l

ppOPT :: MonadReader Env m => TestState -> VM Concrete RealWorld -> [Tx] -> m String
ppOPT (Failed e) _ _  = pure $ "could not evaluate ☣\n  " <> show e
ppOPT Solved     vm l = ppOptimized Nothing vm l
ppOPT Passed     _ _  = pure " passed! 🎉"
ppOPT Open      vm r  = ppOptimized Nothing vm r
ppOPT (Large n) vm l  = do
  m <- asks (.cfg.campaignConf.shrinkLimit)
  ppOptimized (if n < m then Just (n, m) else Nothing) vm l

-- | Pretty-print the status of a optimized test.
ppOptimized :: MonadReader Env m => Maybe (Int, Int) -> VM Concrete RealWorld -> [Tx] -> m String
ppOptimized _ _ []  = pure "Call sequence:\n(no transactions)"
ppOptimized b vm xs = do
  let status = case b of
        Nothing    -> ""
        Just (n,m) -> ", shrinking " <> progress n m
  prettyTxs <- mapM (ppTx vm $ length (nub $ (.src) <$> xs) /= 1) xs
  dappInfo <- asks (.dapp)
  pure $ "\n  Call sequence" <> status <> ":\n"
         <> unlines (("    " <>) <$> prettyTxs) <> "\n"
         <> "Traces: \n" <> T.unpack (showTraceTree dappInfo vm)

-- | Pretty-print the status of all 'SolTest's in a 'Campaign'.
ppTests :: MonadReader Env m => [EchidnaTest] -> m String
ppTests tests = do
  unlines . catMaybes <$> mapM pp tests
  where
  pp t =
    case t.testType of
      PropertyTest n _ -> do
        status <- ppTS t.state (fromJust t.vm) t.reproducer
        pure $ Just (T.unpack n <> ": " <> status)
      CallTest n _ -> do
        status <- ppTS t.state (fromJust t.vm) t.reproducer
        pure $ Just (T.unpack n <> ": " <> status)
      AssertionTest _ s _ -> do
        status <- ppTS t.state (fromJust t.vm) t.reproducer
        pure $ Just (T.unpack (encodeSig s) <> ": " <> status)
      OptimizationTest n _ -> do
        status <- ppOPT t.state (fromJust t.vm) t.reproducer
        pure $ Just (T.unpack n <> ": max value: " <> show t.value <> "\n" <> status)
      Exploration -> pure Nothing

ppTestName :: EchidnaTest -> String
ppTestName t =
  case t.testType of
    PropertyTest n _ -> T.unpack n
    CallTest n _ -> T.unpack n
    AssertionTest _ s _ -> T.unpack (encodeSig s)
    OptimizationTest n _ -> T.unpack n <> ": max value: " <> show t.value
    Exploration -> "<exploration>"

-- | Given a number of boxes checked and a number of total boxes, pretty-print
-- progress in box-checking.
progress :: Int -> Int -> String
progress n m = show n <> "/" <> show m
