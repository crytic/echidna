{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections #-}

module Echidna.Solidity where

import Control.Lens               ((^.), (%=), _1, assign, use, view)
import Control.Exception          (Exception)
import Control.Monad              (liftM2)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.State.Strict (MonadState, execState, modify, runState)
import Data.Foldable              (toList)
import Data.List                  (find, partition)
import Data.Map                   (insert)
import Data.Maybe                 (isNothing)
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf)
import System.Process             (readProcess)
import System.IO.Temp             (writeSystemTempFile)

import qualified Data.Map as Map (lookup)

import Echidna.ABI (SolSignature)

import EVM
  (Contract, VM, VMResult(..), contract, contracts, env, gas, loadContract, replaceCodeOfSelf, resetState, state)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec, vmForEthrunCreation)
import EVM.Keccak   (newContractAddress)
import EVM.Solidity (abiMap, contractName, creationCode, methodInputs, methodName, readSolc, SolcContract)
import EVM.Types    (Addr)

data EchidnaException = BadAddr Addr
                      | CompileFailure
                      | NoContracts
                      | TestArgsFound Text
                      | ContractNotFound Text

instance Show EchidnaException where
  show = \case
    BadAddr a            -> "No contract at " ++ show a ++ " exists"
    CompileFailure       -> "Couldn't compile given file"
    NoContracts          -> "No contracts found in given file"
    (ContractNotFound c) -> "Given contract " ++ show c ++ " not found in given file"
    (TestArgsFound t)    -> "Test " ++ show t ++ " has arguments, aborting"

instance Exception EchidnaException

-- | reads all contracts within the solidity file at `filepath`
readContracts :: (MonadIO m, MonadThrow m) => FilePath -> m [SolcContract]
readContracts filepath = liftIO solc >>= \case
  Nothing -> throwM CompileFailure
  Just m  -> return $ toList $ fst m
  where solc = readSolc =<< writeSystemTempFile "" =<< readProcess
          "solc" ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", filepath] ""

-- | reads either the first contract found or the contract named `selectedContractName` within the solidity file at `filepath`
readContract :: (MonadIO m, MonadThrow m) => FilePath -> Maybe Text -> m SolcContract
readContract filepath selectedContractName = do
    cs <- readContracts filepath
    c <- chooseContract cs selectedContractName
    warn (isNothing selectedContractName && 1 < length cs) $
      "Multiple contracts found in file, only analyzing the first (" <> c ^. contractName <> ")"
    return c
  where chooseContract :: (MonadThrow m) => [SolcContract] -> Maybe Text -> m SolcContract
        chooseContract [] _ = throwM NoContracts
        chooseContract (c:_) Nothing = return c
        chooseContract cs (Just name) = case find (\x -> name == x ^. contractName) cs of
          Nothing -> throwM $ ContractNotFound name
          Just c  -> return c
        warn p s = if p then liftIO $ print s else pure ()

loadSolidity :: (MonadIO m, MonadThrow m) => FilePath -> Maybe Text -> m (VM, [SolSignature], [Text])
loadSolidity filepath selectedContractName = do
    c <- readContract filepath selectedContractName
    let (VMSuccess (B bc), vm) = runState exec . vmForEthrunCreation $ c ^. creationCode
        load = do resetState
                  assign (state . gas) 0xffffffffffffffff
                  loadContract (vm ^. state . contract)
        loaded = execState load $ execState (replaceCodeOfSelf bc) vm
        abi = map (liftM2 (,) (view methodName) (map snd . view methodInputs)) . toList $ c ^. abiMap
        (tests, funs) = partition (isPrefixOf "echidna_" . fst) abi
    case find (not . null . snd) tests of
      Nothing      -> return (loaded, funs, fst <$> tests)
      (Just (t,_)) -> throwM $ TestArgsFound t


insertContract :: MonadState VM m => Contract -> m ()
insertContract c = do a <- (`newContractAddress` 1) <$> use (state . contract)
                      env . contracts %= insert a c
                      modify . execState $ loadContract a

currentContract :: MonadThrow m => VM -> m Contract
currentContract v = let a = v ^. state . contract in
  maybe (throwM $ BadAddr a) pure . Map.lookup a $ v ^. env . contracts

addSolidity :: (MonadIO m, MonadThrow m, MonadState VM m) => FilePath -> Maybe Text -> m ()
addSolidity f m = insertContract =<< currentContract =<< view _1 <$> loadSolidity f m
