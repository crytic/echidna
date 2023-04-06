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
import EVM.Solidity (SolcContract(..))

import Echidna.ABI
import Echidna.Etheno (loadEtheno, extractFromEtheno)
import Echidna.Output.Corpus
import Echidna.Processor
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
-- * A World with all the required data for generating random transactions
-- * A list of Echidna tests to check
-- * A prepopulated dictionary
prepareContract :: Env -> [SolcContract] -> NE.NonEmpty FilePath -> Maybe ContractName -> Seed
                -> IO (VM, World, [EchidnaTest], GenDict)
prepareContract env contracts solFiles specifiedContract seed = do
  let solConf = env.cfg.solConf

  -- compile and load contracts
  (vm, funs, testNames, signatureMap) <- loadSpecified env specifiedContract contracts

  -- load tests
  let echidnaTests = createTests solConf.testMode
                                 solConf.testDestruction
                                 testNames
                                 vm._state._contract
                                 funs

  -- run processors
  slitherInfo <- runSlither (NE.head solFiles) solConf
  case find (< minSupportedSolcVersion) slitherInfo.solcVersions of
    Just outdatedVersion -> throwM $ OutdatedSolcVersion outdatedVersion
    Nothing -> pure ()

  let eventMap = Map.unions $ map (.eventMap) contracts
  let world = mkWorld solConf eventMap signatureMap specifiedContract slitherInfo

  let deployedAddresses = Set.fromList $ AbiAddress <$> Map.keys vm._env._contracts
  let constants = enhanceConstants slitherInfo
                  <> timeConstants
                  <> extremeConstants
                  <> staticAddresses solConf
                  <> deployedAddresses

  let dict = mkGenDict env.cfg.campaignConf.dictFreq
                       -- make sure we don't use cheat codes to form fuzzing call sequences
                       (Set.delete (AbiAddress cheatCode) constants)
                       Set.empty
                       seed
                       (returnTypes contracts)

  pure (vm, world, echidnaTests, dict)

loadInitialCorpus :: Env -> World -> IO [[Tx]]
loadInitialCorpus env world = do
  -- load transactions from init sequence (if any)
  let sigs = Set.fromList $ concatMap NE.toList (HM.elems world.highSignatureMap)
  ethenoCorpus <-
    case env.cfg.solConf.initialize of
      Nothing -> pure []
      Just dir -> do
        ethenos <- loadEtheno dir
        pure [extractFromEtheno ethenos sigs]

  persistedCorpus <-
    case env.cfg.campaignConf.corpusDir of
      Nothing -> pure []
      Just dir -> do
        ctxs1 <- loadTxs (dir </> "reproducers")
        ctxs2 <- loadTxs (dir </> "coverage")
        pure (ctxs1 ++ ctxs2)

  pure $ persistedCorpus ++ ethenoCorpus
