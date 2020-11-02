{-# LANGUAGE FlexibleContexts #-}

module Echidna where

import Control.Lens (view, (^.), to)
import Data.Has (Has(..))
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Fail  (MonadFail(..))
import Control.Monad.Reader (MonadReader, MonadIO, liftIO)
import Control.Monad.Random (MonadRandom)
import Data.Map.Strict (keys)
import Data.Text (pack)

import EVM (env, contracts, VM)
import EVM.ABI (AbiValue(AbiAddress))

import Echidna.ABI
import Echidna.Config
import Echidna.Solidity
import Echidna.Types.Campaign
import Echidna.Types.Random
import Echidna.Types.Tx
import Echidna.Types.World
import Echidna.Transaction
import Echidna.Processor

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
                    Has TxConf x, Has SolConf x)
                => EConfig -> NE.NonEmpty FilePath -> Maybe String -> Seed -> m (VM, World, [SolTest], Maybe GenDict, [[Tx]])
prepareContract cfg fs c g = do
  txs <- liftIO $ loadTxs cd

  -- compile and load contracts
  cs <- Echidna.Solidity.contracts fs
  ads <- addresses
  p <- loadSpecified (pack <$> c) cs

  -- run processors
  ca <- view (hasLens . cryticArgs)
  si <- runSlither (NE.head fs) ca

  -- filter extracted constants
  let cvs = filterConstantValue si

  -- load tests
  (v, w, ts) <- prepareForTest p c si
  let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
  -- start ui and run tests
  return (v, w, ts, Just $ mkGenDict df (cvs ++ timeConstants ++ largeConstants ++ NE.toList ads ++ ads') [] g (returnTypes cs), txs)
  where cd = cfg ^. cConf . corpusDir
        df = cfg ^. cConf . dictFreq
