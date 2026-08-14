module Echidna.Campaign
  ( isSuccessful
  , runWorker
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict (StateT)
import Data.Text (Text)

import EVM.Types hiding (Env)

import Echidna.ABI (GenDict)
import Echidna.Types.Campaign (WorkerState)
import Echidna.Types.Config
import Echidna.Types.Test
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker
import Echidna.Worker.Fuzz (runFuzzWorker)
import Echidna.Worker.Symbolic (runSymWorker)

-- | Given a 'Campaign', check if the test results should be reported as a
-- success or a failure.
isSuccessful :: [EchidnaTest] -> Bool
isSuccessful =
  all (\case { Passed -> True; Open -> True; _ -> False; } . (.state))

runWorker
  :: (MonadIO m, MonadThrow m, MonadReader Env m)
  => WorkerType
  -> StateT WorkerState m ()
  -- ^ Callback to run after each state update (for instrumentation)
  -> VM Concrete -- ^ Initial VM state
  -> GenDict -- ^ Generation dictionary
  -> Int     -- ^ Worker id starting from 0
  -> [(FilePath, [Tx])]
  -- ^ Initial corpus of transactions
  -> Int     -- ^ Test limit for this worker
  -> Maybe Text -- ^ Specified contract name
  -> m (WorkerStopReason, WorkerState)
runWorker SymbolicWorker callback vm dict workerId initialCorpus _ name =
  runSymWorker callback vm dict workerId initialCorpus name
runWorker FuzzWorker callback vm dict workerId initialCorpus testLimit _ =
  runFuzzWorker callback vm dict workerId initialCorpus testLimit
