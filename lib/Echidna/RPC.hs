{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Echidna.RPC where

    import Prelude hiding (Word)
    
    import Control.Exception (Exception)
    import Control.Lens
    import Control.Monad (forM_)
    import Control.Monad.Catch (MonadThrow, throwM)
    import Control.Monad.IO.Class (MonadIO(..))
    import Control.Monad.State.Strict (MonadState, execState, execStateT, get, put, runState)
    import Data.Aeson (FromJSON(..), defaultOptions, eitherDecodeFileStrict, genericParseJSON, omitNothingFields)
    import Data.Has (Has(..))
    import Data.List (partition)
    import Data.Text.Encoding (encodeUtf8)
    import EVM
    import EVM.Concrete (Blob(..), w256)
    import EVM.Exec (exec, vmForEthrunCreation)
    import EVM.Types (Addr, W256)
    import GHC.Generics
    
    import qualified Control.Monad.State.Strict as S (state)
    import qualified Data.Text as T
    
    import Echidna.Exec
    import Echidna.Transaction


    -- | During initialization we can either call a function or create an account or contract
    data EthenoEvent = AccountCreated | ContractCreated | FunctionCall deriving(Eq, Show, Generic)

    instance FromJSON EthenoEvent

    -- | A single initialization event
    data Etheno = Etheno { _event :: !EthenoEvent
                         , _address  :: !Addr
                         , _from  :: !Addr
                         , _to :: !Addr
                         , _contractAddr :: !Addr
                         , _gasUsed :: !Integer
                         , _gasPrice :: !Integer
                         , _initCode :: !T.Text
                         , _value :: !W256
                         } deriving (Eq, Show, Generic)

    makeLenses ''Etheno

    instance FromJSON Etheno where
        parseJSON = genericParseJSON $ defaultOptions{omitNothingFields = True}


    -- | Handler for parsing errors 
    data EthenoException = EthenoException String

    instance Show EthenoException where
        show (EthenoException e) = "Error parsing Etheno initialization file: " ++ e

    instance Exception EthenoException


    -- | Main function: takes a filepath where the initialization sequence lives and returns 
    -- | the initialized VM along with a list of Addr's to put in GenConf
    loadEthenoBatch :: (MonadThrow m, MonadIO m) => FilePath -> m (VM, [Addr])
    loadEthenoBatch fp = do
        bs <- liftIO $ eitherDecodeFileStrict fp

        case bs of 
            (Left e) -> throwM $ EthenoException e
            (Right ethenoInit) -> do
                -- | Separate out account creation txns to use later for config
                let (accounts, txs) = partition (\t -> t ^. event == AccountCreated) ethenoInit
                    knownAddrs      = map (\e -> e ^. address) accounts
        
                -- | Execute contract creations and initial transactions, 
                let initTx  = head txs
                    blank = vmForEthrunCreation $ encodeUtf8 (initTx ^. initCode)
                vm <- execStateT (execEthenoTxs txs) blank

                return (vm, knownAddrs)


    -- | Takes a list of Etheno transactions and loads them into the VM
    execEthenoTxs :: (MonadState x m, Has VM x, MonadThrow m) => [Etheno] -> m ()
    execEthenoTxs txs = forM_ txs $ \t -> do
        og <- get
        setupEthenoTx t 
        res <- liftSH exec
        case (res, t ^. event == ContractCreated) of
            (Reversion,   _)         -> put og
            (VMFailure x, _)         -> vmExcept x
            (VMSuccess (B bc), True) -> hasLens %= execState ( replaceCodeOfSelf bc
                                                            >> loadContract (t ^.contractAddr))
            _                        -> pure ()
        return res


    -- | For an etheno txn, set up VM to execute txn
    setupEthenoTx :: (MonadState x m, Has VM x) => Etheno -> m ()
    setupEthenoTx (Etheno e _ f t c _ _ d v) = S.state . runState . zoom hasLens . sequence_ $
        [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . gas .= 0xffffffff
        , env . origin .= f, state . caller .= f, state . callvalue .= w256 v, setup] where 
        bc = encodeUtf8 d
        setup = case e of 
            AccountCreated -> pure ()
            ContractCreated -> assign (env . contracts . at c) (Just $ initialContract bc) >> loadContract c
            FunctionCall -> loadContract t >> state . calldata .= B bc