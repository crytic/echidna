module Echidna.Deploy where

import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow(..), throwM)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State.Strict (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.Either (fromRight)
import Data.Text (Text, unlines)
import Data.Text.Encoding (encodeUtf8)

import EVM.Solidity
import EVM.Types hiding (Env)

import Echidna.Exec (execTx)
import Echidna.Events (extractEvents)
import Echidna.Types.Config (Env(..))
import Echidna.Types.Solidity (SolException(..))
import Echidna.Types.Tx (createTx, unlimitedGasPerBlock)
import Control.Monad.ST (RealWorld)

deployContracts
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, SolcContract)]
  -> Addr
  -> VM RealWorld
  -> m (VM RealWorld)
deployContracts cs = deployBytecodes' $ map (\(a, c) -> (a, c.creationCode)) cs

deployBytecodes
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, Text)]
  -> Addr
  -> VM RealWorld
  -> m (VM RealWorld)
deployBytecodes cs = deployBytecodes' $
  (\(a, bc) ->
    (a, fromRight (error ("invalid b16 decoding of: " ++ show bc)) $ BS16.decode $ encodeUtf8 bc)
  ) <$> cs

-- | Deploy a list of solidity contracts in certain addresses
deployBytecodes'
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => [(Addr, ByteString)]
  -> Addr
  -> VM RealWorld
  -> m (VM RealWorld)
deployBytecodes' cs src initialVM = foldM deployOne initialVM cs
  where
  deployOne vm (dst, bytecode) = do
    (_, vm') <-
      execTx vm $ createTx (bytecode <> zeros) src dst unlimitedGasPerBlock (0, 0)
    case vm'.result of
      Just (VMSuccess _) -> pure vm'
      _ -> do
        di <- asks (.dapp)
        throwM $ DeploymentFailed dst (Data.Text.unlines $ extractEvents True di vm')
  -- This will initialize with zero a large number of possible constructor parameters
  zeros = BS.replicate 320 0
