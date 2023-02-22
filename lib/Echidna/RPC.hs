{-# LANGUAGE GADTs #-}

module Echidna.RPC where

import Prelude hiding (Word)

import Control.Exception (Exception)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Fail qualified as M (MonadFail(..))
import Control.Monad.State.Strict (MonadState, get, put, execStateT)
import Data.Aeson (FromJSON(..), (.:), withObject, eitherDecodeFileStrict)
import Data.ByteString.Base16 qualified as BS16 (decode)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T (drop)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (member)
import Data.Vector qualified as V (fromList, toList)
import Text.Read (readMaybe)

import EVM
import EVM.ABI (AbiType(..), AbiValue(..), decodeAbiValue, selector)
import EVM.Exec (exec)
import EVM.Types (Addr, W256, Expr(ConcreteBuf))

import Echidna.Exec
import Echidna.Transaction
import Echidna.Types.Signature (SolSignature)
import Echidna.ABI (encodeSig)
import Echidna.Types (fromEVM)
import Echidna.Types.Tx (TxCall(..), Tx(..), makeSingleTx, createTxWithValue, unlimitedGasPerBlock)
import Data.Set (Set)

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

loadEtheno :: FilePath -> IO [Etheno]
loadEtheno fp = do
  bs <- eitherDecodeFileStrict fp
  case bs of
       (Left e) -> throwM $ EthenoException e
       (Right (ethenoInit :: [Etheno])) -> return ethenoInit

extractFromEtheno :: [Etheno] -> Set SolSignature -> [Tx]
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
loadEthenoBatch :: Bool -> FilePath -> IO VM
loadEthenoBatch ffi fp = do
  bs <- eitherDecodeFileStrict fp
  case bs of
    Left e -> throwM $ EthenoException e
    Right (ethenoInit :: [Etheno]) -> do
      -- Execute contract creations and initial transactions,
      let initVM = mapM execEthenoTxs ethenoInit
      execStateT initVM (initialVM ffi)

initAddress :: MonadState VM m => Addr -> m ()
initAddress addr = do
  cs <- use (env . EVM.contracts)
  if addr `member` cs then pure ()
  else env . EVM.contracts . at addr .= Just account
 where account = initialContract (RuntimeCode (ConcreteRuntimeCode mempty)) & set nonce 0 & set balance 100000000000000000000 -- default balance for EOAs in etheno

crashWithQueryError :: (MonadState VM m, MonadFail m, MonadThrow m) => Query -> Etheno -> m ()
crashWithQueryError q et =
  case (q, et) of
    (PleaseFetchContract addr _, FunctionCall f t _ _ _ _) ->
      error ("Address " ++ show addr ++ " was used during function call from " ++ show f ++ " to " ++ show t ++ " but it was never defined as EOA or deployed as a contract")
    (PleaseFetchContract addr _, ContractCreated f t _ _ _ _) ->
      error ("Address " ++ show addr ++ " was used during the contract creation of " ++ show t ++ " from " ++ show f ++ " but it was never defined as EOA or deployed as a contract")
    (PleaseFetchSlot slot _ _, FunctionCall f t _ _ _ _) ->
      error ("Slot " ++ show slot ++ " was used during function call from " ++ show f ++ " to " ++ show t ++ " but it was never loaded")
    (PleaseFetchSlot slot _ _, ContractCreated f t _ _ _ _) ->
      error ("Slot " ++ show slot ++ " was used during the contract creation of " ++ show t ++ " from " ++ show f ++ " but it was never loaded")
    _ -> error $ show (q, et)

-- | Takes a list of Etheno transactions and loads them into the VM, returning the
-- | address containing echidna tests
execEthenoTxs :: (MonadState VM m, MonadFail m, MonadThrow m) => Etheno -> m ()
execEthenoTxs et = do
  setupEthenoTx et
  vm <- get
  res <- fromEVM exec
  case (res, et) of
       (_        , AccountCreated _)  -> return ()
       (Reversion,   _)               -> void $ put vm
       (VMFailure (Query q), _)       -> crashWithQueryError q et
       (VMFailure x, _)               -> vmExcept x >> M.fail "impossible"
       (VMSuccess (ConcreteBuf bc),
        ContractCreated _ ca _ _ _ _) -> do
          env . contracts . at ca . _Just . contractcode .= InitCode mempty mempty
          fromEVM (replaceCodeOfSelf (RuntimeCode (ConcreteRuntimeCode bc)) >> loadContract ca)
          return ()
       _                              -> return ()

-- | For an etheno txn, set up VM to execute txn
setupEthenoTx :: MonadState VM m => Etheno -> m ()
setupEthenoTx (AccountCreated f) = initAddress f -- TODO: improve etheno to include initial balance
setupEthenoTx (ContractCreated f c _ _ d v) = setupTx $ createTxWithValue d f c unlimitedGasPerBlock v (1, 1)
setupEthenoTx (FunctionCall f t _ _ d v) = setupTx $ Tx (SolCalldata d) f t unlimitedGasPerBlock 0 v (1, 1)
setupEthenoTx (BlockMined n t) = setupTx $ Tx NoCall 0 0 0 0 0 (fromInteger t, fromInteger n)
