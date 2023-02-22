module Echidna.Fetch where

import Control.Monad.Catch (MonadThrow(..), throwM)
import Control.Monad.State.Strict (execStateT)
import Data.ByteString (ByteString, pack, append)
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.Either (fromRight)
import Data.Text (Text, unlines)
import Data.Text.Encoding (encodeUtf8)

import EVM
import EVM.Solidity
import EVM.Types (Addr)
import EVM.Dapp (DappInfo)

import Echidna.Types.Solidity (SolException(..))
import Echidna.Types.Tx (createTx, unlimitedGasPerBlock)
import Echidna.Exec (execTx)
import Echidna.Events (extractEvents)

deployContracts :: DappInfo -> [(Addr, SolcContract)] -> Addr -> VM -> IO VM
deployContracts di cs = deployBytecodes' di $ map (\(a, c) -> (a, c.creationCode)) cs

deployBytecodes :: DappInfo -> [(Addr, Text)] -> Addr -> VM -> IO VM
deployBytecodes di cs =
  deployBytecodes' di $ map (\(a, bc) -> (a, fromRight (error ("invalid b16 decoding of: " ++ show bc)) $ BS16.decode $ encodeUtf8 bc)) cs

-- | Deploy a list of solidity contracts in certain addresses
deployBytecodes' :: DappInfo -> [(Addr, ByteString)] -> Addr -> VM -> IO VM
deployBytecodes' _ []            _ vm = return vm
deployBytecodes' di ((a, bc):cs) d vm =
  deployBytecodes' di cs d =<< loadRest
  where
    zeros = pack $ replicate 320 0 -- This will initialize with zero a large number of possible constructor parameters
    loadRest = do
      vm' <- execStateT (execTx $ createTx (bc `append` zeros) d a unlimitedGasPerBlock (0, 0)) vm
      case vm'._result of
        (Just (VMSuccess _)) -> return vm'
        _ -> throwM $ DeploymentFailed a (Data.Text.unlines $ extractEvents True di vm')
