{-# LANGUAGE LambdaCase, TupleSections #-}

module Echidna.Solidity where

import Control.Lens               ((^.), assign, view)
import Control.Exception          (Exception)
import Control.Monad              (liftM2)
import Control.Monad.Catch        (MonadThrow(..))
import Control.Monad.IO.Class     (MonadIO(..))
import Control.Monad.State.Strict (execState, runState)
import Data.Foldable              (toList)
import Data.List                  (find, partition)
import Data.Map                   ()
import Data.Monoid                ((<>))
import Data.Text                  (Text, isPrefixOf)
import System.Process             (readProcess)
import System.IO.Temp             (writeSystemTempFile)

import Echidna.ABI (SolSignature)

import EVM
  (VM, VMResult(..), contract, gas, loadContract, replaceCodeOfSelf, resetState, state)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec, vmForEthrunCreation)
import EVM.Solidity (abiMap, contractName, creationCode, methodInputs, methodName, readSolc)

data EchidnaException = CompileFailure
                      | NoContracts
                      | TestArgsFound Text

instance Show EchidnaException where
  show CompileFailure    = "Couldn't compile given file"
  show NoContracts       = "No contracts found in given file"
  show (TestArgsFound t) = "Test " ++ show t ++ " has arguments, aborting"

instance Exception EchidnaException

loadSolidity :: (MonadIO m, MonadThrow m) => FilePath -> m (VM, [SolSignature], [Text])
loadSolidity f = liftIO solc >>= \case
  Nothing -> throwM CompileFailure
  Just m  -> case toList $ fst m of
    []     -> throwM NoContracts
    (c:cs) -> do
      warn (null cs) $
        "Multiple contracts found in file, only analyzing the first (" <> c ^. contractName <> ")"
      let (VMSuccess (B bc), vm) = runState exec . vmForEthrunCreation $ c ^. creationCode
          load = do resetState
                    assign (state . gas) 0xffffffffffffffff
                    loadContract (vm ^. state . contract)
          loaded = execState load $ execState (replaceCodeOfSelf bc) vm
          abi = map (liftM2 (,) (view methodName) (map snd . view methodInputs)) . toList $ c ^. abiMap
          (tests, funs) = partition (isPrefixOf "echidna_" . fst) abi
      case find (not . null . snd) tests of
        Nothing      -> return (loaded, funs, map fst tests)
        (Just (t,_)) -> throwM $ TestArgsFound t
  where solc = readSolc =<< writeSystemTempFile "" =<< readProcess
          "solc" ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", f] ""
        warn p s = if p then pure () else liftIO $ print s
