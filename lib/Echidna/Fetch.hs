{-# LANGUAGE FlexibleContexts #-}

module Echidna.Fetch where

import Control.Lens
import Control.Monad.Reader       (MonadReader)
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Catch        (MonadThrow(..), throwM)
import Control.Monad.IO.Class     (MonadIO(..))
import Data.Either                (fromRight)
import Data.Has                   (Has(..))

import EVM
import EVM.Solidity
import EVM.Types         (Addr)

import Echidna.Types.Solidity     (SolConf(..), SolException(..))
import Echidna.Types.Tx           (createTx, unlimitedGasPerBlock)
import Echidna.Exec               (execTx)

import Data.ByteString (ByteString, pack, append)
import qualified Data.ByteString.Base16 as BS16 (decode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

-- | Deploy a list of solidity contracts in certain addresses
deployBytecodes' :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
                       => [(Addr, ByteString)] -> Addr -> VM -> m VM
deployBytecodes' []          _ vm = return vm
deployBytecodes' ((a, bc):cs) d vm = deployBytecodes' cs d =<< loadRest
  where zeros = pack $ replicate 320 0 -- This will initialize with zero a large number of possible constructor parameters
        loadRest = do vm' <- execStateT (execTx $ createTx (bc `append` zeros) d a (fromInteger unlimitedGasPerBlock) (0, 0)) vm
                      case vm' ^. result of
                       (Just (VMSuccess _)) -> return vm'
                       _                    -> throwM $ DeploymentFailed a

deployContracts :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
                       => [(Addr, SolcContract)] -> Addr -> VM -> m VM
deployContracts cs = deployBytecodes' $ map (\(a, c) -> (a, c ^. creationCode)) cs

deployBytecodes :: (MonadIO m, MonadThrow m, MonadReader x m, Has SolConf x)
                       => [(Addr, Text)] -> Addr -> VM -> m VM
deployBytecodes cs = deployBytecodes' $ map (\(a, bc) -> (a, fromRight (error ("invalid b16 decoding of: " ++ show bc)) $ BS16.decode $ encodeUtf8 bc)) cs
