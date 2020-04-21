{-# LANGUAGE FlexibleContexts #-}

module Echidna.Top where

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
import Echidna.Transaction
import Echidna.Processor

import qualified Data.List.NonEmpty as NE

prepareContract :: ( MonadCatch m, MonadRandom m, MonadReader x m, MonadIO m, MonadFail m
                   , Has TxConf x, Has SolConf x)
                => EConfig -> NE.NonEmpty FilePath -> Maybe String -> Seed -> m (VM, World, [SolTest], Maybe GenDict, [[Tx]]) 
prepareContract cfg f c g = 
  let cd = cfg ^. cConf . corpusDir
      df = cfg ^. cConf . dictFreq in 
  do
    txs <- liftIO $ loadTxs cd 
    -- compile and load contracts
    cs <- Echidna.Solidity.contracts f
    ads <- addresses
    p <- loadSpecified (pack <$> c) cs

    -- run processors
    ca <- view (hasLens . cryticArgs)
    si <- runSlither (NE.head f) ca

    -- load tests
    (v,w,ts) <- prepareForTest p c si
    let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
    -- start ui and run tests
    return (v,w, ts, Just $ mkGenDict df (extractConstants cs ++ NE.toList ads ++ ads') [] g (returnTypes cs), txs)
