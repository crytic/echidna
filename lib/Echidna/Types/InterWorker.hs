-- | The protocol agents use to talk to each other during a campaign.
--
-- Commands and broadcasts travel over the 'Bus' shared through
-- 'Echidna.Types.Config.Env'. Constraint requests use a separate bounded work
-- queue because they carry VM snapshots and have exactly one consumer.
module Echidna.Types.InterWorker
  ( AgentId(..)
  , BroadcastMsg(..)
  , Bus
  , ConstraintQueue
  , ConstraintRequest(..)
  , FuzzerCmd(..)
  , Message(..)
  , Reply(..)
  , WrappedMessage(..)
  , newConstraintQueueIO
  , submitConstraintRequest
  , takeConstraintRequest
  ) where

import Control.Concurrent.STM
  ( STM, TChan, TMVar, TQueue, TVar, atomically, modifyTVar'
  , newTQueue, newTVar, readTQueue, readTVar, writeTQueue, writeTVar
  )
import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, ViewL(..))
import Data.Sequence qualified as Seq
import Data.Text (Text)

import EVM.Types (VM, VMType(Concrete))

import Echidna.Types.Signature (SolCallPrototype)
import Echidna.Types.Constraint
  (ArgumentBounds, ConstraintKey(..), maxRecentContextsPerSignature)
import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker (WorkerId)

-- | Who sent a message. 'ServerId' is the MCP server, standing in for whoever
-- is driving the campaign from outside it.
data AgentId = FuzzerId WorkerId | SymbolicId | ConstraintDiscoveryId | ServerId
  deriving (Show, Eq, Ord)

-- | The channel a command answers on, filled exactly once by whoever handles
-- the command. Shown opaquely, so that carrying one does not cost a command
-- its derived 'Show'.
newtype Reply a = Reply (TMVar a)

instance Show (Reply a) where
  show _ = "<reply>"

-- | A command addressed to a single fuzzing worker.
data FuzzerCmd
  = EnableSampling Text
    -- ^ Start sampling the given function, named by its canonical signature.
    --   Capped per worker by 'Echidna.Types.Campaign.maxSampledFunctions'.
  | ClearSampling
    -- ^ Forget every sampled function and its statistics.
  | FuzzSequence [SolCallPrototype] Double
    -- ^ Bias generation towards a sequence of calls, using it with the given
    --   probability in place of a corpus-mutated one.
  | ClearPrioritization
    -- ^ Forget every prioritized sequence.
  | SetArgumentBounds ConstraintKey ArgumentBounds
    -- ^ Cache bounds for one exact concrete call context.
  | ProbeArgumentBounds ConstraintKey Tx ArgumentBounds
    -- ^ Replay the captured call context with a small set of boundary values.
    --   Sent only to the fuzzer that requested discovery.
  | ExecuteSequence [Tx] Bool (Reply Text)
    -- ^ Replay a sequence of transactions and report on what it did, without
    --   perturbing the campaign. The flag asks for the EVM trace to be part of
    --   the report; the reply is the JSON report itself. See
    --   'Echidna.Worker.Replay.executeSeq'.
  deriving Show

-- | A non-blocking request for bounds at the concrete state immediately before
-- a call. The worker id is carried by 'WrappedMessage.from'. Keeping the VM in
-- memory is bounded by the small per-signature context cap.
data ConstraintRequest = ConstraintRequest
  { key :: ConstraintKey
  , vm :: VM Concrete
  , targetTx :: Tx
  }

instance Show ConstraintRequest where
  show request = "DiscoverConstraints " <> show request.key.signature

-- | A bounded queue of recent discovery requests. Each fuzzer/signature slot
-- retains only a few VMs, while the FIFO of occupied slots prevents a busy
-- signature from starving the rest.
data ConstraintQueue = ConstraintQueue
  { requests :: TVar (Map (WorkerId, Text) (Seq ConstraintRequest))
  , ready :: TQueue (WorkerId, Text)
  }

newConstraintQueueIO :: IO ConstraintQueue
newConstraintQueueIO = atomically $ ConstraintQueue <$> newTVar Map.empty <*> newTQueue

submitConstraintRequest
  :: ConstraintQueue -> WorkerId -> ConstraintRequest -> STM ()
submitConstraintRequest queue workerId request = do
  current <- readTVar queue.requests
  let slot = (workerId, request.key.signature)
  unless (Map.member slot current) $ writeTQueue queue.ready slot
  modifyTVar' queue.requests $ Map.alter (Just . retainRecent . append) slot
  where
    append Nothing = Seq.singleton request
    append (Just queued) = queued Seq.|> request
    retainRecent queued = Seq.drop
      (max 0 $ Seq.length queued - maxRecentContextsPerSignature)
      queued

takeConstraintRequest :: ConstraintQueue -> STM (WorkerId, ConstraintRequest)
takeConstraintRequest queue = do
  slot@(workerId, _) <- readTQueue queue.ready
  current <- readTVar queue.requests
  case Seq.viewl <$> Map.lookup slot current of
    Just (request :< remaining) -> do
      if Seq.null remaining
        then writeTVar queue.requests $ Map.delete slot current
        else do
          writeTVar queue.requests $ Map.insert slot remaining current
          writeTQueue queue.ready slot
      pure (workerId, request)
    -- Every queued slot is inserted atomically with its request and removed
    -- only here, so this branch is defensive against future protocol changes.
    Just EmptyL -> do
      writeTVar queue.requests $ Map.delete slot current
      takeConstraintRequest queue
    Nothing -> takeConstraintRequest queue

-- | A message every bus subscriber gets to see.
data BroadcastMsg
  = NewCoverageInfo Int [Tx] Bool
    -- ^ Coverage points reached, the sequence that reached them, and whether
    --   it came from replaying the corpus rather than from fuzzing.
  deriving Show

data Message
  = Broadcast BroadcastMsg
  | ToFuzzer WorkerId FuzzerCmd
  deriving Show

-- | A message together with its sender.
data WrappedMessage = WrappedMessage
  { from :: AgentId
  , content :: Message
  } deriving Show

-- | The shared communication bus.
type Bus = TChan WrappedMessage
