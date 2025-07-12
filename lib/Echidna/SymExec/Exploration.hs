{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Echidna.SymExec.Exploration where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReaderT, liftIO)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import EVM.Effects (defaultEnv, defaultConfig, Config(..), Env(..))
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers)
import EVM.SymExec (IterConfig(..), LoopHeuristic (..), VeriOpts(..), defaultVeriOpts)
import EVM.Types (abiKeccak, FunctionSelector, VMType(..))
import qualified EVM.Types (VM(..))
import Control.Monad.ST (RealWorld)

import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (Env(..), EConfig(..), OperationMode(..), OutputFormat(..), UIConf(..))
import Echidna.Types.Random (shuffleIO)
import Echidna.Types.World (World(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Echidna.SymExec.Common (suitableForSymExec, exploreMethod, rpcFetcher, TxOrError(..), PartialsLogs)

-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
-- Also takes an optional Tx argument; this is used as the transaction
--   to symbolize. If none is provided, we do full
--   symbolic execution.
--   The Tx argument, if present, must have a .call value of type SolCall.

-- | Filters methods based on the campaign configuration.
-- If symExecTargets is Just, it filters methods by their name.
-- If symExecTargets is Nothing, it filters methods by their signature and checks if they are suitable for symbolic execution.
-- If the list of assertSigs is empty, it does not filter by signature (to workaround a potential bug in Slither).
filterTarget :: Maybe [Text] -> [FunctionSelector] -> Maybe Tx -> Method -> Bool
filterTarget symExecTargets assertSigs tx method =
  case (symExecTargets, tx) of
    (Just ms, _)                                       -> method.name `elem` ms
    (_,  Just (Tx { call = SolCall (methodName, _) })) -> (null assertSigs || methodSig `elem` assertSigs) && method.name == methodName && suitableForSymExec method
    _                                                  -> (null assertSigs || methodSig `elem` assertSigs) && suitableForSymExec method
 where methodSig = abiKeccak $ encodeUtf8 method.methodSignature

exploreContract :: (MonadIO m, MonadThrow m, MonadReader Echidna.Types.Config.Env m) => SolcContract -> Maybe Tx -> EVM.Types.VM Concrete RealWorld -> m (ThreadId, MVar ([TxOrError], PartialsLogs))
exploreContract contract tx vm = do
  conf <- asks (.cfg)
  env <- ask
  contractCacheRef <- asks (.fetchContractCache)
  slotCacheRef <- asks (.fetchSlotCache)
  let
    allMethods = Map.elems contract.abiMap
    filteredMethods = filter (filterTarget conf.campaignConf.symExecTargets env.world.assertSigs tx) allMethods
    methods = filteredMethods
    timeoutSMT = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = Just conf.campaignConf.symExecMaxIters
    maxExplore = Just (fromIntegral conf.campaignConf.symExecMaxExplore)
    askSmtIters = conf.campaignConf.symExecAskSMTIters
    rpcInfo = rpcFetcher conf.rpcUrl (fromIntegral <$> conf.rpcBlock)
    defaultSender = fromJust $ fmap (.dst) tx <|> Set.lookupMin conf.solConf.sender <|> Just 0

  threadIdChan <- liftIO newEmptyMVar
  doneChan <- liftIO newEmptyMVar
  resultChan <- liftIO newEmptyMVar
  let iterConfig = IterConfig { maxIter = maxIters, askSmtIters = askSmtIters, loopHeuristic = Naive}
  let veriOpts = defaultVeriOpts {iterConf = iterConfig, rpcInfo = rpcInfo}
  let isNonInteractive = conf.uiConf.operationMode == NonInteractive Text
  let runtimeEnv = defaultEnv { config = defaultConfig { maxWidth = 5, maxDepth = maxExplore, maxBufSize = 12, promiseNoReent = True, debug = isNonInteractive, dumpQueries = False, numCexFuzz = 100 } }

  liftIO $ flip runReaderT runtimeEnv $ withSolvers conf.campaignConf.symExecSMTSolver (fromIntegral conf.campaignConf.symExecNSolvers) 1 timeoutSMT $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT runtimeEnv $ do
      shuffleMethods <- liftIO $ shuffleIO methods
      -- For now, we will be exploring a single method at a time.
      -- In some cases, this methods list will have only one method, but in other cases, it will have several methods.
      -- This is to improve the user experience, as it will produce results more often, instead having to wait for exploring several
      res <- forM (take 1 shuffleMethods) $ \method -> exploreMethod method contract vm defaultSender conf veriOpts solvers rpcInfo contractCacheRef slotCacheRef
      liftIO $ putMVar resultChan (concatMap fst res, concatMap snd res)
      --liftIO $ print "done"
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- liftIO $ takeMVar threadIdChan
  pure (threadId, resultChan)