module Echidna.Orphans.Rand () where

-- THIS MODULE EXPORTS NO FUNCTIONS
-- IT ONLY EXPORTS ORPHAN INSTANCES

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Random.Strict (RandT)
import Control.Monad.Trans (lift)

-- | Workers run their body in @RandT g (StateT WorkerState m)@ and call into
-- 'Echidna.Execution.callseq', which needs 'MonadThrow'. @exceptions@ covers
-- the standard transformers, but MonadRandom's 'RandT' is a newtype over
-- 'StateT' that derives nothing from @exceptions@, hence the gap we fill here.
-- Any further lifted instances for 'RandT' (MonadCatch or MonadMask, should a
-- worker ever need @bracket@) belong in this module too.
instance MonadThrow m => MonadThrow (RandT g m) where
  throwM = lift . throwM
