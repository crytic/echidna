{-# LANGUAGE FlexibleContexts #-}

module Echidna where

import Control.Lens (view, (^.), to)
import Data.Has (Has(..))
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Reader (MonadReader, MonadIO, liftIO)
import Control.Monad.Random (MonadRandom)
import Data.Map.Strict (keys)
import Data.HashMap.Strict (toList)
import Data.List (nub)

import EVM (env, contracts, VM)
import EVM.ABI (AbiValue(AbiAddress))
import EVM.Solidity (SourceCache, SolcContract)

import Echidna.ABI
import Echidna.Types.Config hiding (cfg)
import Echidna.Solidity
import Echidna.Types.Solidity
import Echidna.Types.Campaign
import Echidna.Types.Random
import Echidna.Types.Signature
import Echidna.Types.Test
import Echidna.Types.Tx
import Echidna.Types.World
import Echidna.Processor
import Echidna.Output.Corpus
import Echidna.RPC (loadEtheno, extractFromEtheno)

import qualified Data.List.NonEmpty as NE

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
-- * A prepopulated dictionary (if any)
-- * A list of transaction sequences to initialize the corpus
prepareContract :: (MonadCatch m, MonadRandom m, MonadReader x m, MonadIO m, MonadFail m,
                    Has TestConf x, Has TxConf x, Has SolConf x)
                => EConfig -> NE.NonEmpty FilePath -> Maybe ContractName -> Seed
                -> m (VM, SourceCache, [SolcContract], World, [EchidnaTest], Maybe GenDict, [[Tx]])
prepareContract cfg fs c g = do
  ctxs <- liftIO $ loadTxs cd

  -- compile and load contracts
  (cs, scs) <- Echidna.Solidity.contracts fs
  p <- loadSpecified c cs

  -- run processors
  ca <- view (hasLens . cryticArgs)
  si <- runSlither (NE.head fs) ca

  -- load tests
  (v, w, ts) <- prepareForTest p c si

  -- get signatures
  let sigs = nub $ concatMap (NE.toList . snd) (toList $ w ^. highSignatureMap)

  ads <- addresses
  let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
  let constants' = enhanceConstants si ++ timeConstants ++ largeConstants ++ NE.toList ads ++ ads'

  -- load transactions from init sequence (if any)
  es' <- liftIO $ maybe (return []) loadEtheno it
  let txs = ctxs ++ maybe [] (const [extractFromEtheno es' sigs]) it

  -- start ui and run tests
  let sc = selectSourceCache c scs
  return (v, sc, cs, w, ts, Just $ mkGenDict df constants' [] g (returnTypes cs), txs)
  where cd = cfg ^. cConf . corpusDir
        df = cfg ^. cConf . dictFreq
        it = cfg ^. sConf . initialize
