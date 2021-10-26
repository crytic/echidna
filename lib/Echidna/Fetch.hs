{-# LANGUAGE FlexibleContexts #-}

module Echidna.Fetch where

import Control.Lens
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Data.Has                   (Has(..))

import EVM
import EVM.Solidity
import EVM.Types         (Addr)

import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Tx           (TxConf, createTx, createTxWithValue, unlimitedGasPerBlock, initialTimestamp, initialBlockNumber)
import Echidna.Exec               (execTx)

-- | Load a list of solidity contracts as libraries
--loadLibraries :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
--              => [SolcContract] -> Addr -> Addr -> VM -> m VM
--loadLibraries []     _  _ vm = return vm
--loadLibraries (l:ls) la d vm = loadLibraries ls (la + 1) d =<< loadRest
--  where loadRest = execStateT (execTx $ createTx (l ^. creationCode) d la (fromInteger unlimitedGasPerBlock) (0, 0)) vm

-- | Deploy a list of solidity contracts in certain addresses
deployContracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
                       => [(Addr, SolcContract)] -> Addr -> VM -> m VM
deployContracts []          _ vm = return vm
deployContracts ((a, c):cs) d vm = deployContracts cs d =<< loadRest
  where loadRest = execStateT (execTx $ createTx (c ^. creationCode) d a (fromInteger unlimitedGasPerBlock) (0, 0)) vm
