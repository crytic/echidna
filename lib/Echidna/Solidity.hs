{-# LANGUAGE TemplateHaskell, FlexibleContexts, OverloadedStrings, LambdaCase, TupleSections #-}


module Echidna.Solidity where

import Control.Lens               ((^.), (%=), use, view, makeLenses)
import Control.Exception          (Exception)
import Control.Monad              (liftM2)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.Reader       (MonadReader, ask)
import Control.Monad.State.Strict (MonadState, execState, modify)
import Data.Aeson.Types           (Value)
import Data.ByteString            (ByteString)
import Data.Foldable              (toList)
import Data.List                  (find, partition)
import Data.Map                   (Map, insert)
import Data.Maybe                 (isNothing, fromMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf, pack, unpack)
import System.Process             (readProcess)
import System.IO.Temp             (writeSystemTempFile)

import qualified Data.Map as Map (lookup)

import Echidna.ABI    (SolSignature)
import Echidna.Config (Config(..), prefix, solcArgs, ignored)


import EVM
  (Contract, VM, contract, contracts, env, state, Error, loadContract)
import EVM.Keccak   (newContractAddress)
import EVM.Solidity (abiMap, contractName, creationCode, constructorInputs, methodInputs, methodName, readSolc, SolcContract, eventMap)
import EVM.Types    (Addr, W256)
import EVM.ABI      (Event, AbiType)

data TestableContract = TestableContract
  { _ctorCode      :: ByteString
  , _constructor   :: [(Text, AbiType)]
  , _functions     :: [SolSignature]
  , _tests         :: [Text]
  , _events        :: Map W256 Event 
  , _config        :: Config 
  }
  deriving Show

instance Show VM where
  show _ = "<no show for VM>"

makeLenses ''TestableContract


data EchidnaException = BadAddr Addr
                      | CompileFailure
                      | ConstructorFailure EVM.Error
                      | NoContracts
                      | TestArgsFound Text
                      | ContractNotFound Text
                      | NoBytecode Text
                      | NoFuncs
                      | NoTests
                      | OnlyTests
                      | ParseValueError AbiType Value String

instance Show EchidnaException where
  show = \case
    BadAddr a             -> "No contract at " ++ show a ++ " exists"
    CompileFailure        -> "Couldn't compile given file"
    (ConstructorFailure e)-> "Constructor encoutered an error during its execution: " ++ show e
    NoContracts           -> "No contracts found in given file"
    (ContractNotFound c)  -> "Given contract " ++ show c ++ " not found in given file"
    (TestArgsFound t)     -> "Test " ++ show t ++ " has arguments, aborting"
    (NoBytecode t)        -> "No bytecode found for contract " ++ show t
    NoFuncs               -> "ABI is empty, are you sure your constructor is right?"
    NoTests               -> "No tests found in ABI"
    OnlyTests             -> "Only tests and no public functions found in ABI"
    ParseValueError t v s -> "Failed to parse " ++ show v ++ " as " ++ show t ++ ": " ++ s

instance Exception EchidnaException

-- | parses additional solc arguments
solcArguments :: FilePath -> Maybe Text -> [String]
solcArguments filePath argStr = args <> fromMaybe [] additional
  where args = ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", filePath]
        additional = words . unpack <$> argStr

-- | reads all contracts within the solidity file at `filepath` and passes optional solc params to compiler
readContracts :: (MonadIO m, MonadThrow m, MonadReader Config m) => FilePath -> m [SolcContract]
readContracts filePath = do
  conf <- ask
  liftIO (solc conf) >>= \case
    Nothing -> throwM CompileFailure
    Just m  -> return $ toList $ fst m
  where solc c = readSolc =<< writeSystemTempFile "" =<< readProcess
          "solc" (solcArguments filePath (pack <$> (c ^. solcArgs))) ""

-- | reads either the first contract found or the contract named `selectedContractName` within the solidity file at `filepath`
readContract :: (MonadIO m, MonadThrow m, MonadReader Config m) => FilePath -> Maybe Text -> m SolcContract
readContract filePath selectedContractName = do
    cs <- readContracts filePath
    c <- chooseContract cs $ ((pack filePath <> ":") <>) <$> selectedContractName
    warn (isNothing selectedContractName && 1 < length cs)
      "Multiple contracts found in file, only analyzing the first"
    liftIO $ print $ "Analyzing contract: " <> c ^. contractName
    return c
  where chooseContract :: (MonadThrow m) => [SolcContract] -> Maybe Text -> m SolcContract
        chooseContract [] _ = throwM NoContracts
        chooseContract (c:_) Nothing = return c
        chooseContract cs (Just name) = case find (\x -> name == x ^. contractName) cs of
          Nothing -> throwM $ ContractNotFound name
          Just c  -> return c
        warn :: (MonadIO m) => Bool -> Text -> m ()
        warn p s = if p then liftIO $ print s else pure ()

loadSolidity :: (MonadIO m, MonadThrow m, MonadReader Config m)
             => FilePath
             -> Maybe Text
             -> m TestableContract --(VM, [SolSignature], [Text])
loadSolidity filePath selectedContract = do
    conf <- ask
    c    <- readContract filePath selectedContract
    let abi = map (liftM2 (,) (view methodName) (map snd . view methodInputs)) . toList $ c ^. abiMap
        (ts, funs) = partition (isPrefixOf (conf ^. prefix) . fst) abi
        funs' = filter ((\e -> not $ e `elem` (conf ^. ignored))  . fst) funs
    if null abi then throwM NoFuncs else pure ()
    if null funs then throwM OnlyTests else pure ()
    case find (not . null . snd) ts of
      Nothing      -> return $
          TestableContract
          (c ^. creationCode)
          (c ^. constructorInputs)
          funs'
          (fst <$> ts)
          (view eventMap c)
          conf
      (Just (t,_)) -> throwM $ TestArgsFound t


insertContract :: MonadState VM m => Contract -> m ()
insertContract c = do a <- (`newContractAddress` 1) <$> use (state . contract)
                      env . contracts %= insert a c
                      modify . execState $ loadContract a

currentContract :: MonadThrow m => VM -> m Contract
currentContract v = let a = v ^. state . contract in
  maybe (throwM $ BadAddr a) pure . Map.lookup a $ v ^. env . contracts
