{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Monad.State.Strict (MonadState, execStateT, runStateT, get, put, runState)
import Data.Aeson (FromJSON(..), (.:), withObject, eitherDecodeFileStrict)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Char8 (ByteString, empty)
import Data.Has (Has(..))
import Data.List (partition)
import Data.Map (fromList)
import Data.Text.Encoding (encodeUtf8)
import EVM
import EVM.ABI (AbiType(..), getAbi)
import EVM.Concrete (w256)
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Types (Addr, W256)

import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Control.Monad.State.Strict as S (state)
import qualified Data.ByteString.Base16 as BS16 (decode)
import qualified Data.Text as T (Text, drop)
import qualified Data.Vector as V (fromList)

import Echidna.Exec
import Echidna.Transaction

-- | During initialization we can either call a function or create an account or contract
data Etheno = AccountCreated Addr
            | ContractCreated Addr Addr Integer Integer ByteString W256
            | FunctionCall Addr Addr Integer Integer ByteString W256
  deriving (Eq, Show)

instance FromJSON Etheno where
  -- parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = tail, omitNothingFields = True}
  parseJSON = withObject "Etheno" $ \v -> do
    (ev :: String) <- v .: "event"
    case ev of
         "AccountCreated"  -> AccountCreated  <$> v .: "address"
         "ContractCreated" -> ContractCreated <$> v .: "from"
                                              <*> v .: "contract_address"
                                              <*> (read <$> (v .: "gas_used"))
                                              <*> (read <$> (v .: "gas_price"))
                                              <*> (decode <$> (v .: "data"))
                                              <*> v .: "value"
         "FunctionCall"    -> FunctionCall    <$> v .: "from"
                                              <*> v .: "to"
                                              <*> (read <$> (v .: "gas_used"))
                                              <*> (read <$> (v .: "gas_price"))
                                              <*> (decode <$> (v .: "data"))
                                              <*> v .: "value"
         _ -> M.fail "event should be one of \"AccountCreated\", \"ContractCreated\", or \"FunctionCall\""
    where decode :: T.Text -> ByteString
          decode = decodeHex . encodeUtf8 . T.drop 2
          decodeHex = fst . BS16.decode


-- | Handler for parsing errors
data EthenoException = EthenoException String

instance Show EthenoException where
    show (EthenoException e) = "Error parsing Etheno initialization file: " ++ e

instance Exception EthenoException


-- | Main function: takes a filepath where the initialization sequence lives and returns
-- | the initialized VM along with a list of Addr's to put in GenConf
loadEthenoBatch :: (MonadThrow m, MonadIO m, Has TxConf y, MonadReader y m) => [T.Text] -> FilePath -> m (VM, [Addr])
loadEthenoBatch ts fp = do
  bs <- liftIO $ eitherDecodeFileStrict fp

  case bs of
       (Left e) -> throwM $ EthenoException e
       (Right ethenoInit) -> do
         -- Separate out account creation txns to use later for config
         let (accounts, txs) = partition (\case { AccountCreated{} -> True; _ -> False; }) ethenoInit
             knownAddrs      = map (\(AccountCreated a) -> a) accounts

         -- Execute contract creations and initial transactions,
         let blank  = vmForEthrunCreation empty & env . contracts .~ fromList []
             initVM = foldM (execEthenoTxs ts) Nothing txs

         (addr, vm') <- runStateT initVM blank
         case addr of
              Nothing -> throwM $ EthenoException "Could not find a contract with echidna tests"
              Just a  -> do
                vm <- execStateT (liftSH . loadContract $ a) vm'
                return (vm, knownAddrs)

-- | Takes a list of Etheno transactions and loads them into the VM, returning the
-- | address containing echidna tests
execEthenoTxs :: (MonadState x m, Has VM x, MonadThrow m, Has TxConf y, MonadReader y m) => [T.Text] -> Maybe Addr -> Etheno -> m (Maybe Addr)
execEthenoTxs ts addr et = do
  setupEthenoTx et
  res <- liftSH exec
  g <- view (hasLens . propGas)
  case (res, et) of
       (Reversion,   _)               -> throwM $ EthenoException "Encountered reversion while setting up Etheno transactions"
       (VMFailure x, _)               -> vmExcept x >> return addr
       (VMSuccess bc,
        ContractCreated _ ca _ _ _ _) -> do
          hasLens . env . contracts . at ca . _Just . contractcode .= InitCode ""
          liftSH (replaceCodeOfSelf (RuntimeCode bc) >> loadContract ca)
          og <- get
          -- See if current contract is the same as echidna test
          case addr of
               Just m  -> return $ Just m
               Nothing -> let txs = ts <&> \t -> Tx (Left (t, [])) ca ca g 0 0 (0,0)
                              go []     = return (Just ca)
                              go (x:xs) = setupTx x >> liftSH exec >>= \case
                                VMSuccess r -> do
                                  put og
                                  case runGetOrFail (getAbi . AbiTupleType . V.fromList $ [AbiBoolType]) (r ^. lazy) ^? _Right . _3 of
                                       Just _  -> go xs
                                       Nothing -> return Nothing
                                _           -> put og >> return Nothing in
                            go txs
       _                              -> return addr


-- | For an etheno txn, set up VM to execute txn
setupEthenoTx :: (MonadState x m, Has VM x) => Etheno -> m ()
setupEthenoTx (AccountCreated _) = pure ()
setupEthenoTx (ContractCreated f c _ _ d v) = S.state . runState . zoom hasLens . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . gas .= 0xffffffff
  , tx . origin .= f, state . caller .= f, state . callvalue .= w256 v, setup]
  where setup = assign (env . contracts . at c) (Just . initialContract . RuntimeCode $ d) >> loadContract c

setupEthenoTx (FunctionCall f t _ _ d v) = S.state . runState . zoom hasLens . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . gas .= 0xffffffff
  , tx . origin .= f, state . caller .= f, state . callvalue .= w256 v, setup]
  where setup = loadContract t >> state . calldata .= d