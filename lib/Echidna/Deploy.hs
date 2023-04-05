module Echidna.Deploy where

import Control.Monad.Catch (MonadThrow(..), throwM)
import Control.Monad.State.Strict (execStateT, MonadIO)
import Data.ByteString (ByteString, pack, append)
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.Either (fromRight)
import Data.Text (Text, unlines)
import Data.Text.Encoding (encodeUtf8)

import EVM hiding (Env)
import EVM.Solidity
import EVM.Types (Addr)

import Echidna.Types.Solidity (SolException(..))
import Echidna.Types.Tx (createTx, unlimitedGasPerBlock)
import Echidna.Exec (execTx)
import Echidna.Events (extractEvents)
import Control.Monad.Reader (MonadReader, asks)
import Echidna.Types.Config (Env(..))

deployContracts
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, SolcContract)] -> Addr -> VM -> m VM
deployContracts cs = deployBytecodes' $ map (\(a, c) -> (a, c.creationCode)) cs

deployBytecodes
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, Text)] -> Addr -> VM -> m VM
deployBytecodes cs =
  deployBytecodes' $ map (\(a, bc) -> (a, fromRight (error ("invalid b16 decoding of: " ++ show bc)) $ BS16.decode $ encodeUtf8 bc)) cs

-- | Deploy a list of solidity contracts in certain addresses
deployBytecodes'
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, ByteString)] -> Addr -> VM -> m VM
deployBytecodes' []            _ vm = return vm
deployBytecodes' ((a, bc):cs) d vm =
  deployBytecodes' cs d =<< loadRest
  where
    zeros = pack $ replicate 320 0 -- This will initialize with zero a large number of possible constructor parameters
    loadRest = do
      vm' <- execStateT (execTx $ createTx (bc `append` zeros) d a unlimitedGasPerBlock (0, 0)) vm
      case vm'.result of
        (Just (VMSuccess _)) -> return vm'
        _ -> do
          di <- asks (.dapp)
          throwM $ DeploymentFailed a (Data.Text.unlines $ extractEvents True di vm')
