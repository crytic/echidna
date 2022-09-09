{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Echidna.RPC where

import Prelude hiding (Word)

import Control.Exception (Exception)
import Control.Lens
import Control.Monad (foldM, void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Strict (MonadState, runStateT, get, put)
import Data.Aeson (FromJSON(..), (.:), withObject, eitherDecodeFileStrict)
import Data.ByteString.Char8 (ByteString)
import Data.Has (Has(..))
import Data.Text.Encoding (encodeUtf8)
import Data.Map (member)

import EVM
import EVM.ABI (AbiType(..), AbiValue(..), decodeAbiValue, selector)
import EVM.Exec (exec)
import EVM.Types (Addr, Buffer(..), W256, w256)
import Text.Read (readMaybe)

import qualified Control.Monad.Fail as M (MonadFail(..))
import qualified Data.ByteString.Base16 as BS16 (decode)
import qualified Data.Text as T (drop)
import qualified Data.Vector as V (fromList, toList)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Echidna.Exec
import Echidna.Transaction
import Echidna.Types.Signature (SolSignature)
import Echidna.ABI (encodeSig)

import Echidna.Types.Tx (TxCall(..), Tx(..), TxConf, makeSingleTx, createTxWithValue, unlimitedGasPerBlock)

-- | During initialization we can either call a function or create an account or contract
data Etheno = AccountCreated Addr                                       -- ^ Registers an address with the echidna runtime
            | ContractCreated Addr Addr Integer Integer ByteString W256 -- ^ A contract was constructed on the blockchain
            | FunctionCall Addr Addr Integer Integer ByteString W256    -- ^ A contract function was executed
            | BlockMined Integer Integer                                -- ^ A new block was mined contract

  deriving (Eq, Show)

instance FromJSON Etheno where
  parseJSON = withObject "Etheno" $ \v -> do
    (ev :: String) <- v .: "event"
    let gu = maybe (M.fail "could not parse gas_used") pure . readMaybe =<< v .: "gas_used"
        gp = maybe (M.fail "could not parse gas_price") pure . readMaybe =<< v .: "gas_price"
        ni = maybe (M.fail "could not parse number_increase") pure . readMaybe =<< v .: "number_increment"
        ti = maybe (M.fail "could not parse timestamp_increase") pure . readMaybe =<< v .: "timestamp_increment"
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
         "BlockMined"      -> BlockMined      <$> ni
                                              <*> ti

         _ -> M.fail "event should be one of \"AccountCreated\", \"ContractCreated\", or \"FunctionCall\""
    where decode x = case BS16.decode . encodeUtf8 . T.drop 2 $ x of
                          Right a -> pure a
                          Left e  -> M.fail $ "could not decode hexstring: " <> e


-- | Handler for parsing errors
-- TODO: make this a better sum type
newtype EthenoException = EthenoException String

instance Show EthenoException where
    show (EthenoException e) = "Error parsing Etheno initialization file: " ++ e

instance Exception EthenoException

loadEtheno :: (MonadThrow m, MonadIO m, M.MonadFail m)
                => FilePath -> m [Etheno]
loadEtheno fp = do
  bs <- liftIO $ eitherDecodeFileStrict fp

  case bs of
       (Left e) -> throwM $ EthenoException e
       (Right (ethenoInit :: [Etheno])) -> return ethenoInit

extractFromEtheno :: [Etheno] -> [SolSignature] -> [Tx]
extractFromEtheno ess ss = case ess of
                           (BlockMined ni ti :es)  -> Tx NoCall 0 0 0 0 0 (fromInteger ti, fromInteger ni) : extractFromEtheno es ss
                           (c@FunctionCall{} :es)  -> concatMap (`matchSignatureAndCreateTx` c) ss ++ extractFromEtheno es ss
                           (_:es)                  -> extractFromEtheno es ss
                           _                       -> []

matchSignatureAndCreateTx :: SolSignature -> Etheno -> [Tx]
matchSignatureAndCreateTx ("", []) _ = [] -- Not sure if we should match this.
matchSignatureAndCreateTx (s,ts) (FunctionCall a d _ _ bs v) =
  if BS.take 4 bs == selector (encodeSig (s,ts))
  then makeSingleTx a d v $ SolCall (s, fromTuple $ decodeAbiValue t (LBS.fromStrict $ BS.drop 4 bs))
  else []
  where t = AbiTupleType (V.fromList ts)
        fromTuple (AbiTuple xs) = V.toList xs
        fromTuple _            = []
matchSignatureAndCreateTx _ _                                = []

-- | Main function: takes a filepath where the initialization sequence lives and returns
-- | the initialized VM along with a list of Addr's to put in GenConf
loadEthenoBatch :: (MonadThrow m, MonadIO m, Has TxConf y, MonadReader y m, M.MonadFail m)
                => FilePath -> m VM
loadEthenoBatch fp = do
  bs <- liftIO $ eitherDecodeFileStrict fp

  case bs of
       (Left e) -> throwM $ EthenoException e
       (Right (ethenoInit :: [Etheno])) -> do
         -- Execute contract creations and initial transactions,
         let initVM = foldM execEthenoTxs () ethenoInit
         (_, vm') <- runStateT initVM initialVM
         return vm'

initAddress :: (MonadState s m, Has VM s) => Addr -> m ()
initAddress addr = do
  cs <- use (hasLens . env . EVM.contracts)
  if addr `member` cs then return ()
  else hasLens . env . EVM.contracts . at addr .= Just account
 where account = initialContract (RuntimeCode mempty) & set nonce 0 & set balance (w256 0xffffffff) -- default balance for EOA

-- | Takes a list of Etheno transactions and loads them into the VM, returning the
-- | address containing echidna tests
execEthenoTxs :: (MonadState x m, Has VM x, MonadThrow m, Has TxConf y, MonadReader y m, M.MonadFail m)
              => () -> Etheno -> m ()
execEthenoTxs _ et = do
  setupEthenoTx et
  sb <- get
  res <- liftSH exec
  case (res, et) of
       (_        , AccountCreated _)  -> return ()
       (Reversion,   _)               -> void $ put sb
       (VMFailure x, _)               -> vmExcept x >> M.fail "impossible"
       (VMSuccess (ConcreteBuffer bc),
        ContractCreated _ ca _ _ _ _) -> do
          hasLens . env . contracts . at ca . _Just . contractcode .= InitCode (ConcreteBuffer "")
          liftSH (replaceCodeOfSelf (RuntimeCode (ConcreteBuffer bc)) >> loadContract ca)
          return ()
       _                              -> return ()

-- | For an etheno txn, set up VM to execute txn
setupEthenoTx :: (MonadState x m, Has VM x) => Etheno -> m ()
setupEthenoTx (AccountCreated f) = initAddress f
setupEthenoTx (ContractCreated f c _ _ d v) = setupTx $ createTxWithValue d f c (fromInteger unlimitedGasPerBlock) (w256 v) (1, 1)
setupEthenoTx (FunctionCall f t _ _ d v) = do
   setupTx $ Tx (SolCalldata d) f t (fromInteger unlimitedGasPerBlock) 0 (w256 v) (1, 1)
setupEthenoTx (BlockMined n t) = setupTx $ Tx NoCall 0 0 0 0 0 (fromInteger t, fromInteger n)
