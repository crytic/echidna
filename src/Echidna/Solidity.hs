{-# LANGUAGE LambdaCase, TupleSections #-}

module Echidna.Solidity where

import Control.Lens
import Control.Exception (Exception)
import Control.Monad (liftM2)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict hiding (state)
import Data.Foldable (toList)
import Data.Map ()
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Process (readProcess)
import System.IO.Temp (writeSystemTempFile)

import EVM hiding (Error)
import EVM.ABI
import EVM.Concrete
import EVM.Exec
import EVM.Solidity

data EchidnaException = CompileFailure | NoContracts

instance Show EchidnaException where
  show CompileFailure = "Couldn't compile given file"
  show NoContracts    = "No contracts found in given file"

instance Exception EchidnaException

loadSolidity :: (MonadIO m, MonadThrow m) => FilePath -> m (VM, [(Text, [AbiType])])
loadSolidity f = liftIO solc >>= \case
  Nothing -> throwM CompileFailure
  Just m  -> case toList $ fst m of
    [] -> throwM NoContracts
    (c:cs) -> do
      if null cs then pure () else liftIO . print $
          "Multiple contracts found in file, only analyzing the first (" <> c ^. contractName <> ")"
      let (VMSuccess (B bc), vm) = runState exec . vmForEthrunCreation $ c ^. creationCode
          load = do resetState
                    assign (state . gas) 0xffffffffffffffff
                    loadContract (vm ^. state . contract)
          loaded = execState load $ execState (replaceCodeOfSelf bc) vm
          abi = map (liftM2 (,) _methodName (map snd . _methodInputs)) . toList $ c ^. abiMap
      return (loaded, abi)
  where solc = readSolc =<< writeSystemTempFile "" =<< readProcess
          "solc" ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", f] ""
