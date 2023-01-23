module Echidna where

import Control.Monad.Catch (MonadThrow(..))
import Data.HashMap.Strict qualified as HM
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.FilePath ((</>))

import EVM
import EVM.ABI (AbiValue(AbiAddress))
import EVM.Solidity (SourceCache, SolcContract)

import Echidna.ABI
import Echidna.Types.Config hiding (cfg)
import Echidna.Types.Solidity
import Echidna.Types.Campaign
import Echidna.Types.Random
import Echidna.Types.Signature
import Echidna.Types.Test
import Echidna.Types.Tx
import Echidna.Types.World
import Echidna.Solidity
import Echidna.Processor
import Echidna.Output.Corpus
import Echidna.RPC (loadEtheno, extractFromEtheno)

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
-- * A list of transaction sequences to initialize the corpus
prepareContract :: EConfig -> NE.NonEmpty FilePath -> Maybe ContractName -> Seed
                -> IO (VM, SourceCache, [SolcContract], World, [EchidnaTest], GenDict, [[Tx]])
prepareContract cfg fs c g = do
  ctxs <- case cfg.campaignConf.corpusDir of
            Nothing -> pure []
            Just dir -> do
              ctxs1 <- loadTxs (dir </> "reproducers")
              ctxs2 <- loadTxs (dir </> "coverage")
              pure (ctxs1 ++ ctxs2)

  let solConf = cfg.solConf

  -- compile and load contracts
  (cs, scs) <- Echidna.Solidity.contracts solConf fs
  p <- loadSpecified solConf c cs

  -- run processors
  si <- runSlither (NE.head fs) solConf.cryticArgs
  case find (< minSupportedSolcVersion) si.solcVersions  of
    Just outdatedVersion -> throwM $ OutdatedSolcVersion outdatedVersion
    Nothing -> return ()

  -- load tests
  let (vm, world, ts) = prepareForTest solConf p c si

  -- get signatures
  let sigs = Set.fromList $ concatMap NE.toList (HM.elems world.highSignatureMap)

  let ads = addresses solConf
  let ads' = AbiAddress <$> Map.keys vm._env._contracts
  let constants' = Set.fromList $ enhanceConstants si ++
                                  timeConstants ++
                                  extremeConstants ++
                                  Set.toList ads ++
                                  ads'

  -- load transactions from init sequence (if any)
  ethenoCorpus <-
    case cfg.solConf.initialize of
      Nothing -> pure []
      Just fp -> do
        es' <- loadEtheno fp
        pure [extractFromEtheno es' sigs]

  let corp = ctxs ++ ethenoCorpus

  let sc = selectSourceCache c scs

  let dict = mkGenDict cfg.campaignConf.dictFreq constants' Set.empty g (returnTypes cs)

  pure (vm, sc, cs, world, ts, dict, corp)
