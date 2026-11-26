{-# LANGUAGE GADTs #-}

-- | Human-readable progress without retaining explored VMs or flooding the
-- campaign event queue. Heartbeats remain responsive during long SMT queries.
module Echidna.SymExec.Progress (formatProgress, withProgress) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Monad (forever, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import UnliftIO.Async (withAsync)

import EVM.ABI (callSig)
import EVM.Invariant (CallProgress(..), Target(..))
import EVM.Invariant.Driver (Progress(..))
import EVM.Types (EType(EAddr), Expr(..))

formatProgress :: Int -> Progress -> Text
formatProgress bound = \case
  ExploringCall{depth, stateIndex, stateCount, prefix, currentCall} ->
    "tx " <> position depth bound <> statePosition stateIndex stateCount
      <> case currentCall.target of
        Nothing -> " | NoCall (advance time/block)" <> preceding prefix
        Just target -> " | function " <> position currentCall.functionIndex currentCall.functionCount
          <> ": " <> targetName target
          <> maybe "" (\a -> " | caller " <> address a) currentCall.caller
          <> preceding prefix
  LevelFinished{depth, reachedStates} ->
    "tx " <> position depth bound <> " complete | " <> tshow reachedStates <> " states reached"
  CheckingInvariant{depth, stateIndex, stateCount, prefix, checkTarget} ->
    "checking " <> targetName checkTarget <> " after " <> position depth bound <> " txs"
      <> statePosition stateIndex stateCount <> preceding prefix
  where
    position n total = tshow n <> "/" <> tshow total
    statePosition n total = " | state " <> position n total
    targetName :: Target -> Text
    targetName target = (if T.null target.contractName then "" else target.contractName <> ".")
      <> callSig target.sig
    preceding [] = ""
    preceding calls = " | prefix: " <> T.intercalate " -> " (map (maybe "NoCall" targetName) calls)
    address :: Expr EAddr -> Text
    address (LitAddr addr) = tshow addr
    address expr = tshow expr

tshow :: Show a => a -> Text
tshow = T.pack . show

data ActiveProgress = ActiveProgress
  { message :: !Text
  , started :: !Word64
  }

-- | Log at most one ordinary update per second, plus depth boundaries and a
-- heartbeat every five seconds. The reporter is cancelled before returning or
-- propagating an exception, including campaign timeouts.
withProgress :: Int -> (String -> IO ()) -> ((Progress -> IO ()) -> IO a) -> IO a
withProgress bound emit action = do
  started <- getMonotonicTimeNSec
  state <- newMVar (Nothing, 0)
  let seconds now since = tshow ((now - since) `div` 1_000_000_000) <> "s"
      publish now current message = emit $ T.unpack $ message
        <> maybe "" (\since -> " | current call " <> seconds now since) current
        <> " | elapsed " <> seconds now started
      observe progress = do
        modifyMVar_ state $ \(previous, lastPrinted) -> do
          now <- getMonotonicTimeNSec
          -- Force the rendered Text now: queued messages must not keep the
          -- driver's call prefix (and its symbolic expressions) alive.
          let !active = ActiveProgress (formatProgress bound progress) now
              completed = case progress of LevelFinished{} -> True; _ -> False
          let first = case previous of Nothing -> True; _ -> False
              printNow = completed || first || now - lastPrinted >= 1_000_000_000
          when printNow $ publish now (if completed then Nothing else Just now) active.message
          pure (if completed then Nothing else Just active, if printNow then now else lastPrinted)
      heartbeat = forever $ do
        threadDelay 5_000_000
        modifyMVar_ state $ \(active, lastPrinted) -> do
          now <- getMonotonicTimeNSec
          case active of
            Just current | now - lastPrinted >= 1_000_000_000 -> do
              publish now (Just current.started) current.message
              pure (active, now)
            _ -> pure (active, lastPrinted)
  withAsync heartbeat $ \_ -> action observe
