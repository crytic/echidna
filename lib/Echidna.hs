module Echidna where

import Control.Monad.Catch (MonadThrow(..))
import Data.HashMap.Strict qualified as HM
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.FilePath ((</>))

import EVM hiding (Env, env, contracts)
import EVM.ABI (AbiValue(AbiAddress))
import EVM.Solidity (SourceCache, SolcContract(..))

import Echidna.ABI
import Echidna.Output.Corpus
import Echidna.Processor
import Echidna.RPC (loadEtheno, extractFromEtheno)
import Echidna.Solidity
import Echidna.Test (createTests)
import Echidna.Types.Campaign hiding (corpus)
import Echidna.Types.Config
import Echidna.Types.Random
import Echidna.Types.Signature
import Echidna.Types.Solidity
import Echidna.Types.Test
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
-- * A World with all the required data for generating random transctions
-- * A list of Echidna tests to check
-- * A prepopulated dictionary
prepareContract :: Env -> NE.NonEmpty FilePath -> Maybe ContractName -> Seed
                -> IO (VM, SourceCache, [SolcContract], World, [EchidnaTest], GenDict)
prepareContract env solFiles specifiedContract seed = do
  let solConf = env.cfg.solConf

  -- compile and load contracts
  (contracts, scs) <- compileContracts solConf solFiles
  (vm, funs, testNames, signatureMap) <- loadSpecified env specifiedContract contracts

  -- run processors
  slitherInfo <- runSlither (NE.head solFiles) solConf.cryticArgs
  case find (< minSupportedSolcVersion) slitherInfo.solcVersions of
    Just outdatedVersion -> throwM $ OutdatedSolcVersion outdatedVersion
    Nothing -> pure ()

  -- load tests
  let echidnaTests = createTests solConf.testMode
                                 solConf.testDestruction
                                 testNames
                                 vm._state._contract
                                 funs

  let eventMap = Map.unions $ map (.eventMap) contracts
  let world = mkWorld solConf eventMap signatureMap specifiedContract slitherInfo

  let deployedAddresses = Set.fromList $ AbiAddress <$> Map.keys vm._env._contracts
  let constants = enhanceConstants slitherInfo
                  <> timeConstants
                  <> extremeConstants
                  <> staticAddresses solConf
                  <> deployedAddresses

  let dict = mkGenDict env.cfg.campaignConf.dictFreq
                       constants
                       Set.empty
                       seed
                       (returnTypes contracts)

  pure (vm, selectSourceCache specifiedContract scs, contracts, world, echidnaTests, dict)

prepareCorpus :: Env -> World -> IO [[Tx]]
prepareCorpus env world = do
  -- load transactions from init sequence (if any)
  let sigs = Set.fromList $ concatMap NE.toList (HM.elems world.highSignatureMap)
  ethenoCorpus <-
    case env.cfg.solConf.initialize of
      Nothing -> pure []
      Just fp -> do
        es <- loadEtheno fp
        pure [extractFromEtheno es sigs]

  persistedCorpus <-
    case env.cfg.campaignConf.corpusDir of
      Nothing -> pure []
      Just dir -> do
        ctxs1 <- loadTxs (dir </> "reproducers")
        ctxs2 <- loadTxs (dir </> "coverage")
        pure (ctxs1 ++ ctxs2)

  pure $ persistedCorpus ++ ethenoCorpus
