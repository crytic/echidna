module Echidna.Deploy where

import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow(..), throwM)
import Control.Monad.Reader (MonadReader, asks, liftIO)
import Control.Monad.State.Strict (execStateT, MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.Either (fromRight)
import Data.Text (Text, unlines)
import Data.Text.Encoding (encodeUtf8)

import EVM hiding (bytecode, Env)
import EVM.Solidity
import EVM.Types (Addr)

import Echidna.Exec (execTx)
import Echidna.Events (extractEvents)
import Echidna.Types.Config (Env(..))
import Echidna.Types.Solidity (SolException(..))
import Echidna.Types.Tx (createTx, unlimitedGasPerBlock)
import qualified Data.Map as Map
import GHC.IORef (atomicModifyIORef')

deployContracts
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, SolcContract)]
  -> Addr
  -> VM
  -> m VM
deployContracts cs = deployBytecodes' $ map (\(a, c) -> (a, c.creationCode)) cs

deployBytecodes
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, Text)]
  -> Addr
  -> VM
  -> m VM
deployBytecodes cs = deployBytecodes' $
  (\(a, bc) ->
    (a, fromRight (error ("invalid b16 decoding of: " ++ show bc)) $ BS16.decode $ encodeUtf8 bc)
  ) <$> cs

-- | Deploy a list of solidity contracts in certain addresses
deployBytecodes'
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, ByteString)]
  -> Addr
  -> VM
  -> m VM
deployBytecodes' cs src initialVM = foldM deployOne initialVM cs
  where
  deployOne vm (dst, bytecode) = do
    bytecodesRef <- asks (.bytecodesRef)
    vm' <- flip execStateT vm $
      execTx $ createTx (bytecode <> zeros) src dst unlimitedGasPerBlock (0, 0)
    case vm'.result of
      Just (VMSuccess _) -> do
        liftIO $ atomicModifyIORef' bytecodesRef $ \bytecodes -> (Map.insert dst bytecode bytecodes, ())
        pure vm'
      _ -> do
        di <- asks (.dapp)
        throwM $ DeploymentFailed dst (Data.Text.unlines $ extractEvents True di vm')
  -- This will initialize with zero a large number of possible constructor parameters
  zeros = BS.replicate 320 0
