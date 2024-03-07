module Echidna where

import Control.Concurrent (newChan)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.ST (RealWorld)
import Data.IORef (writeIORef, newIORef)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.FilePath ((</>))

import EVM (cheatCode)
import EVM.ABI (AbiValue(AbiAddress))
import EVM.Dapp (DappInfo(..), dappInfo)
import EVM.Fetch qualified
import EVM.Solidity (BuildOutput)
import EVM.Types hiding (Env)

import Echidna.ABI
import Echidna.Etheno (loadEtheno, extractFromEtheno)
import Echidna.Output.Corpus
import Echidna.SourceAnalysis.Slither
import Echidna.Solidity
import Echidna.Symbolic (forceAddr)
import Echidna.Test (createTests)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Random
import Echidna.Types.Signature
import Echidna.Types.Solidity
import Echidna.Types.Tx
import Echidna.Types.World

-- | This function is used to prepare, process, compile and initialize smart contracts for testing.
-- It takes:
-- * A config record
-- * A list of contract files paths for the smart contract code
-- * A contract name (if any)
-- * A seed used during the random generation
-- and returns:
-- * A VM with the contract deployed and ready for testing
-- * A World with all the required data for generating random transactions
-- * A list of Echidna tests to check
-- * A prepopulated dictionary
prepareContract
  :: Env
  -> NonEmpty FilePath
  -> Maybe ContractName
  -> Seed
  -> IO (VM Concrete RealWorld, World, GenDict)
prepareContract env solFiles specifiedContract seed = do
  let solConf = env.cfg.solConf
      contracts = Map.elems env.dapp.solcByName

  -- deploy contracts
  (vm, funs, testNames, signatureMap) <- loadSpecified env specifiedContract contracts

  -- run processors
  slitherInfo <- runSlither (NE.head solFiles) solConf
  case find (< minSupportedSolcVersion) slitherInfo.solcVersions of
    Just version | detectVyperVersion version -> pure ()
    Just version                              -> throwM $ OutdatedSolcVersion version
    Nothing -> pure ()

  let
    -- load tests
    echidnaTests = createTests solConf.testMode
                               solConf.testDestruction
                               testNames
                               (forceAddr vm.state.contract)
                               funs

    world = mkWorld solConf signatureMap specifiedContract slitherInfo contracts

    deployedAddresses = Set.fromList $ AbiAddress . forceAddr <$> Map.keys vm.env.contracts
    constants = enhanceConstants slitherInfo
                <> timeConstants
                <> extremeConstants
                <> staticAddresses solConf
                <> deployedAddresses

    dict = mkGenDict env.cfg.campaignConf.dictFreq
                     -- make sure we don't use cheat codes to form fuzzing call sequences
                     (Set.delete (AbiAddress $ forceAddr cheatCode) constants)
                     Set.empty
                     seed
                     (returnTypes contracts)

  writeIORef env.testsRef echidnaTests
  pure (vm, world, dict)

loadInitialCorpus :: Env -> World -> IO [(FilePath, [Tx])]
loadInitialCorpus env world = do
  -- load transactions from init sequence (if any)
  let sigs = Set.fromList $ concatMap NE.toList (Map.elems world.highSignatureMap)
  ethenoCorpus <-
    case env.cfg.solConf.initialize of
      Nothing -> pure []
      Just dir -> do
        ethenos <- loadEtheno dir
        pure [(dir, extractFromEtheno ethenos sigs)]

  persistedCorpus <-
    case env.cfg.campaignConf.corpusDir of
      Nothing -> pure []
      Just dir -> do
        ctxs1 <- loadTxs (dir </> "reproducers")
        ctxs2 <- loadTxs (dir </> "coverage")
        pure (ctxs1 ++ ctxs2)

  pure $ persistedCorpus ++ ethenoCorpus

mkEnv :: EConfig -> BuildOutput -> IO Env
mkEnv cfg buildOutput = do
  fetchContractCache <- newIORef mempty
  fetchSlotCache <- newIORef mempty
  codehashMap <- newIORef mempty
  chainId <- maybe (pure Nothing) EVM.Fetch.fetchChainIdFrom cfg.rpcUrl
  eventQueue <- newChan
  coverageRef <- newIORef mempty
  corpusRef <- newIORef mempty
  testsRef <- newIORef mempty
  -- TODO put in real path
  let dapp = dappInfo "/" buildOutput
  pure $ Env { cfg, dapp, codehashMap, fetchContractCache, fetchSlotCache
             , chainId, eventQueue, coverageRef, corpusRef, testsRef
             }
