module Echidna.Types where

import Control.Exception (Exception)
import Control.Monad.State.Strict (MonadState, get, put, MonadIO(liftIO), runStateT)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Text (Text, unpack)
import Data.Word (Word64)
import EVM (initialContract)
import EVM.Types

-- | We throw this when our execution fails due to something other than reversion.
-- The `Maybe Text` on `UnknownFailure` is an optional stack trace.
data ExecException = IllegalExec EvmError | UnknownFailure EvmError (Maybe Text)

instance Show ExecException where
  show = \case
    IllegalExec e -> "VM attempted an illegal operation: " ++ show e
    UnknownFailure (MaxCodeSizeExceeded limit actual) _ ->
      "Max code size exceeded. " ++ codeSizeErrorDetails limit actual
    UnknownFailure (MaxInitCodeSizeExceeded limit actual) _ ->
      "Max init code size exceeded. " ++ codeSizeErrorDetails limit actual
    UnknownFailure e trace -> "VM failed for unhandled reason, " ++ show e
      ++ ". This shouldn't happen. Please file a ticket with this error message and steps to reproduce!"
      ++ maybe "" ((" Stack trace:\n" ++) . unpack) trace
    where
      codeSizeErrorDetails limit actual =
        "Configured limit: " ++ show limit ++ ", actual: " ++ show actual
        ++ ". Set 'codeSize: 0xffffffff' in the config file to increase the limit."

instance Exception ExecException

type Gas = Word64

type MutationConsts a = (a, a, a, a)

-- | Transform an EVM action from HEVM to our MonadState VM
fromEVM :: (MonadIO m, MonadState (VM Concrete RealWorld) m) => EVM Concrete RealWorld r -> m r
fromEVM evmAction = do
  vm <- get
  (result, vm') <- liftIO $ stToIO $ runStateT evmAction vm
  put vm'
  pure result

emptyAccount :: Contract
emptyAccount = initialContract (RuntimeCode (ConcreteRuntimeCode mempty))
