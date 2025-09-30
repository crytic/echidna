module Echidna where

import Control.Concurrent (newChan)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (liftIO)
import Network.Wreq.Session qualified as NetSession
import Control.Monad.ST (RealWorld)
import Data.IORef (newIORef)
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import System.FilePath ((</>))

import EVM (cheatCode)
import EVM.ABI (AbiValue(AbiAddress))
import EVM.Dapp (dappInfo)
import EVM.Fetch qualified
import EVM.Solidity (BuildOutput(..), Contracts(Contracts), Method(..), Mutability(..), SolcContract(..))
import EVM.Types hiding (Env)

import Echidna.ABI
import Echidna.Onchain as Onchain
import Echidna.Output.Corpus
import Echidna.SourceMapping (findSrcForReal)
import Echidna.SourceAnalysis.Slither
import Echidna.Solidity
import Echidna.SymExec.Symbolic (forceAddr)
import Echidna.Types.Campaign
import Echidna.Types.Config
import Echidna.Types.Random
import Echidna.Types.Solidity
import Echidna.Types.Tx
import Echidna.Types.World
import Echidna.Types.Test (EchidnaTest)
import Echidna.Types.Signature (ContractName)

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
  :: EConfig
  -> NonEmpty FilePath
  -> BuildOutput
  -> Maybe ContractName
  -> Seed
  -> IO (VM Concrete RealWorld, Env, GenDict)
prepareContract cfg solFiles buildOutput selectedContract seed = do
  let solConf = cfg.solConf
      (Contracts contractMap) = buildOutput.contracts
      contracts = Map.elems contractMap

  mainContract <- selectMainContract solConf selectedContract contracts
  tests <- mkTests solConf mainContract
  signatureMap <- mkSignatureMap solConf mainContract contracts

  -- run processors
  slitherInfo <- runSlither (NE.head solFiles) solConf
  case find (< minSupportedSolcVersion) slitherInfo.solcVersions of
    Just version | detectVyperVersion version -> pure ()
    Just version                              -> throwM $ OutdatedSolcVersion version
    Nothing -> pure ()

  let world = mkWorld cfg.solConf signatureMap selectedContract slitherInfo contracts

  env <- mkEnv cfg buildOutput tests world (Just slitherInfo)

  -- deploy contracts
  vm <- loadSpecified env mainContract contracts
  let
    deployedAddresses = Set.fromList $ AbiAddress . forceAddr <$> Map.keys vm.env.contracts
    constants = enhanceConstants slitherInfo
                <> timeConstants
                <> extremeConstants
                <> staticAddresses solConf
                <> deployedAddresses
    deployedSolcContracts = nub $ mapMaybe (findSrcForReal env.dapp) $ Map.elems vm.env.contracts
    nonViewPureSigs = concatMap (mapMaybe (\ (Method {name, inputs, mutability}) -> 
      case mutability of
        View -> Nothing
        Pure -> Nothing
        Payable -> Just (name, map snd inputs)
        NonPayable -> Just (name, map snd inputs))
      . Map.elems . (\ (SolcContract {abiMap}) -> abiMap)) deployedSolcContracts
    dict = mkGenDict env.cfg.campaignConf.dictFreq
                     -- make sure we don't use cheat codes to form fuzzing call sequences
                     (Set.delete (AbiAddress $ forceAddr cheatCode) constants)
                     Set.empty
                     seed
                     (returnTypes contracts)
                     nonViewPureSigs
  pure (vm, env, dict)

loadInitialCorpus :: Env -> IO [(FilePath, [Tx])]
loadInitialCorpus env = do
  case env.cfg.campaignConf.corpusDir of
    Nothing -> pure []
    Just dir -> do
      ctxs1 <- loadTxs (dir </> "reproducers")
      ctxs2 <- loadTxs (dir </> "coverage")
      pure (ctxs1 ++ ctxs2)

mkEnv :: EConfig -> BuildOutput -> [EchidnaTest] -> World -> Maybe SlitherInfo -> IO Env
mkEnv cfg buildOutput tests world slitherInfo = do
  codehashMap <- newIORef mempty
  chainId <- maybe (pure Nothing) Onchain.fetchChainIdFrom (Just cfg.rpcUrl)
  eventQueue <- newChan
  coverageRefInit <- newIORef mempty
  coverageRefRuntime <- newIORef mempty
  corpusRef <- newIORef mempty
  testRefs <- traverse newIORef tests
  -- Create session manually since mkSession needs App context
  sess <- liftIO NetSession.newAPISession
  let emptyCache = EVM.Fetch.FetchCache Map.empty Map.empty Map.empty
  cache <- liftIO $ newMVar emptyCache
  latestBlockNum <- liftIO $ newMVar Nothing
  let fetchSession = EVM.Fetch.Session sess latestBlockNum cache
  contractNameCache <- newIORef mempty
  -- TODO put in real path
  let dapp = dappInfo "/" buildOutput
  pure $ Env { cfg, dapp, codehashMap, fetchSession, contractNameCache
             , chainId, eventQueue, coverageRefInit, coverageRefRuntime, corpusRef, testRefs, world
             , slitherInfo
             }
