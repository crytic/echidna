module Echidna.Types where

import Control.Exception (Exception)
import Control.Monad.State.Strict (MonadState, get, put, MonadIO(liftIO), runStateT)
import Control.Monad.ST (RealWorld, stToIO)
import Data.Word (Word64)
import EVM (initialContract)
import EVM.Types

-- | We throw this when our execution fails due to something other than reversion.
data ExecException = IllegalExec EvmError | UnknownFailure EvmError

instance Show ExecException where
  show (IllegalExec e) = "VM attempted an illegal operation: " ++ show e
  show (UnknownFailure e) = "VM failed for unhandled reason, " ++ show e
    ++ ". This shouldn't happen. Please file a ticket with this error message and steps to reproduce!"

instance Exception ExecException

type Gas = Word64

type MutationConsts a = (a, a, a, a)

-- | Transform an EVM action from HEVM to our MonadState VM
fromEVM :: (MonadIO m, MonadState (VM RealWorld) m) => EVM RealWorld r -> m r
fromEVM evmAction = do
  vm <- get
  (result, vm') <- liftIO $ stToIO $ runStateT evmAction vm
  put vm'
  pure result

emptyAccount :: Contract
emptyAccount = initialContract (RuntimeCode (ConcreteRuntimeCode mempty))
