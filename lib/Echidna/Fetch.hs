{-# LANGUAGE FlexibleContexts #-}

module Echidna.Fetch where

import Control.Lens
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Catch        (MonadThrow(..), throwM)
import Control.Monad.IO.Class     (MonadIO(..))
import Data.Has                   (Has(..))

import EVM
import EVM.Solidity
import EVM.Types         (Addr)

import Echidna.Types.Solidity     (SolConf(..), SolException(..))
import Echidna.Types.Tx           (createTx, unlimitedGasPerBlock)
import Echidna.Exec               (execTx)

import Data.ByteString (pack, append)

-- | Deploy a list of solidity contracts in certain addresses
deployContracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
                       => [(Addr, SolcContract)] -> Addr -> VM -> m VM
deployContracts []          _ vm = return vm
deployContracts ((a, c):cs) d vm = deployContracts cs d =<< loadRest
  where zeros = pack $ replicate 320 0 -- This will initialize with zero a large number of possible constructor parameters
        loadRest = do vm' <- execStateT (execTx $ createTx ((c ^. creationCode) `append` zeros) d a (fromInteger unlimitedGasPerBlock) (0, 0)) vm
                      case vm' ^. result of
                       (Just (VMSuccess _)) -> return vm'
                       _                    -> throwM $ DeploymentFailed a
