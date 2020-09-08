{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.RPC where

import Prelude hiding (Word)

import Control.Exception (Exception)
import Control.Lens
import Control.Monad (foldM)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Strict (MonadState, execStateT, runStateT, get, put)
import Data.Aeson (FromJSON(..), (.:), withObject, eitherDecodeFileStrict)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Char8 (ByteString, empty)
import Data.Has (Has(..))
import Data.Map (fromList)
import Data.Text.Encoding (encodeUtf8)
import EVM
import EVM.ABI (AbiType(..), getAbi)
import EVM.Concrete (w256)
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Types (Addr, W256)
import EVM.Symbolic (Buffer(..))
import Text.Read (readMaybe)

import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Data.ByteString.Base16 as BS16 (decode)
import qualified Data.Text as T (Text, drop, unpack)
import qualified Data.Vector as V (fromList)

import Echidna.Exec
import Echidna.Transaction
import Echidna.Types.Tx (TxCall(..), Tx(Tx), TxConf, propGas)

-- | During initialization we can either call a function or create an account or contract
data Etheno = AccountCreated Addr                                       -- ^ Registers an address with the echidna runtime
            | ContractCreated Addr Addr Integer Integer ByteString W256 -- ^ A contract was constructed on the blockchain
            | FunctionCall Addr Addr Integer Integer ByteString W256    -- ^ A contract function was executed
  deriving (Eq, Show)

instance FromJSON Etheno where
  parseJSON = withObject "Etheno" $ \v -> do
    (ev :: String) <- v .: "event"
    let gu = maybe (M.fail "could not parse gas_used") pure . readMaybe =<< v .: "gas_used"
        gp = maybe (M.fail "could not parse gas_price") pure . readMaybe =<< v .: "gas_price"
    case ev of
         "AccountCreated"  -> AccountCreated  <$> v .: "address"
         "ContractCreated" -> ContractCreated <$> v .: "from"
                                              <*> v .: "contract_address"
                                              <*> gu
                                              <*> gp
                                              <*> (decode =<< (v .: "data"))
                                              <*> v .: "value"
         "FunctionCall"    -> FunctionCall    <$> v .: "from"
                                              <*> v .: "to"
                                              <*> gu
                                              <*> gp
                                              <*> (decode =<< (v .: "data"))
                                              <*> v .: "value"
         _ -> M.fail "event should be one of \"AccountCreated\", \"ContractCreated\", or \"FunctionCall\""
    where decode x = case BS16.decode . encodeUtf8 . T.drop 2 $ x of
                          (a, "") -> pure a
                          _       -> M.fail $ "could not decode hexstring: " <> T.unpack x


-- | Handler for parsing errors
-- TODO: make this a better sum type
newtype EthenoException = EthenoException String

instance Show EthenoException where
    show (EthenoException e) = "Error parsing Etheno initialization file: " ++ e

instance Exception EthenoException

-- | Main function: takes a filepath where the initialization sequence lives and returns
-- | the initialized VM along with a list of Addr's to put in GenConf
loadEthenoBatch :: (MonadThrow m, MonadIO m, Has TxConf y, MonadReader y m, M.MonadFail m)
                => [T.Text] -> FilePath -> m VM
loadEthenoBatch ts fp = do
  bs <- liftIO $ eitherDecodeFileStrict fp

  case bs of
       (Left e) -> throwM $ EthenoException e
       (Right (ethenoInit :: [Etheno])) -> do
         -- Execute contract creations and initial transactions,
         let blank  = vmForEthrunCreation empty & env . contracts .~ fromList []
             initVM = foldM (execEthenoTxs ts) Nothing ethenoInit

         (addr, vm') <- runStateT initVM blank
         case addr of
              Nothing -> throwM $ EthenoException "Could not find a contract with echidna tests"
              Just a  -> execStateT (liftSH . loadContract $ a) vm'

-- | Takes a list of Etheno transactions and loads them into the VM, returning the
-- | address containing echidna tests
execEthenoTxs :: (MonadState x m, Has VM x, MonadThrow m, Has TxConf y, MonadReader y m, M.MonadFail m)
              => [T.Text] -> Maybe Addr -> Etheno -> m (Maybe Addr)
execEthenoTxs ts addr et = do
  setupEthenoTx et
  res <- liftSH exec
  g <- view (hasLens . propGas)
  case (res, et) of
       (Reversion,   _)               -> throwM $ EthenoException "Encountered reversion while setting up Etheno transactions"
       (VMFailure x, _)               -> vmExcept x >> M.fail "impossible"
       (VMSuccess (ConcreteBuffer bc),
        ContractCreated _ ca _ _ _ _) -> do
          hasLens . env . contracts . at ca . _Just . contractcode .= InitCode ""
          liftSH (replaceCodeOfSelf (RuntimeCode bc) >> loadContract ca)
          og <- get
          -- See if current contract is the same as echidna test
          case addr of
               -- found the tests, so just return the contract
               Just m  -> return $ Just m
               -- try to see if this is the contract we wish to test
               Nothing -> let txs = ts <&> \t -> Tx (SolCall (t, [])) ca ca g 0 0 (0,0)
                              -- every test was executed successfully
                              go []     = return (Just ca)
                              -- execute x and check if it returned something of the correct type
                              go (x:xs) = setupTx x >> liftSH exec >>= \case
                                -- executing the test function succeeded
                                VMSuccess (ConcreteBuffer r) -> do
                                  put og
                                  case runGetOrFail (getAbi . AbiTupleType . V.fromList $ [AbiBoolType]) (r ^. lazy) ^? _Right . _3 of
                                       -- correct type ==> check the rest of the tests
                                       Just _  -> go xs
                                       -- incorrect type ==> bad ABI, this is not the contract we wish to test
                                       Nothing -> return Nothing
                                -- some vm failure or reversion, not what we want
                                -- TODO: this breaks any test that is supposed to revert, maybe add a check here?
                                _           -> put og >> return Nothing in
                            -- actually test everything
                            go txs
       _                              -> return addr


-- | For an etheno txn, set up VM to execute txn
setupEthenoTx :: (MonadState x m, Has VM x) => Etheno -> m ()
setupEthenoTx (AccountCreated _) = pure ()
setupEthenoTx (ContractCreated f c _ _ d v) = setupTx $ Tx (SolCreate d) f c 0xffffffff 0 (w256 v) (0, 0)
setupEthenoTx (FunctionCall f t _ _ d v) = setupTx $ Tx (SolCalldata d) f t 0xffffffff 0 (w256 v) (0, 0)
