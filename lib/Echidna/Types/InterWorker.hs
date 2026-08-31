-- | The protocol agents use to talk to each other during a campaign.
--
-- Everything travels over a single 'Bus' shared through 'Echidna.Types.Config.Env'.
-- The bus is a broadcast channel, so a message is delivered to every reader
-- rather than raced for: readers call 'Control.Concurrent.STM.dupTChan' to get
-- their own view of it, and filter for what concerns them.
module Echidna.Types.InterWorker
  ( AgentId(..)
  , BroadcastMsg(..)
  , Bus
  , FuzzerCmd(..)
  , Message(..)
  , WrappedMessage(..)
  ) where

import Control.Concurrent.STM (TChan)
import Data.Text (Text)

import Echidna.Types.Tx (Tx)
import Echidna.Types.Worker (WorkerId)

-- | Who sent a message.
data AgentId = FuzzerId WorkerId | SymbolicId
  deriving (Show, Eq, Ord)

-- | A command addressed to a single fuzzing worker.
data FuzzerCmd
  = EnableSampling Text
    -- ^ Start sampling the given function, named by its canonical signature.
    --   Capped per worker by 'Echidna.Types.Campaign.maxSampledFunctions'.
  | ClearSampling
    -- ^ Forget every sampled function and its statistics.
  deriving Show

-- | A message every agent gets to see.
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
