-- | Replaying a concrete transaction sequence to report on it, rather than to
-- fuzz with it.
module Echidna.Worker.Replay (executeSeq) where

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import Data.List qualified as List
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Word (Word64)

import EVM.Format (showTraceTree)
import EVM.Types (Block(..), VM(..), VMType(Concrete), forceLit)

import Echidna.Events (Events, extractEvents)
import Echidna.Exec (execTx)
import Echidna.Test (checkAssertionEvent, checkPanicEvent)
import Echidna.Types.Config (Env(..))
import Echidna.Types.Tx (Tx, TxResult(..), getResult)
import Echidna.UI.Report (ppTx)

-- | How a single transaction of the sequence ended. Assertion failures are
-- kept apart from ordinary reverts because they mean different things: a
-- revert is usually just an input the contract rejected, an assertion failure
-- is the contract contradicting itself.
data TxStatus = Completed | Reverted | AssertionFailed
  deriving Eq

instance ToJSON TxStatus where
  toJSON Completed = "completed"
  toJSON Reverted = "reverted"
  toJSON AssertionFailed = "assertion_failed"

-- | What replaying one transaction produced.
data TxOutcome = TxOutcome
  { index :: Int -- ^ Position in the sequence, counting from one
  , call :: String
  , status :: TxStatus
  , result :: TxResult
  , gasUsed :: Word64
  , logs :: Events
  }

instance ToJSON TxOutcome where
  toJSON outcome = object
    [ "index" .= outcome.index
    , "call" .= outcome.call
    , "status" .= outcome.status
    , "result" .= outcome.result
    , "gas_used" .= outcome.gasUsed
    , "logs" .= outcome.logs
    ]

-- | Replay a concrete sequence of transactions and describe what happened as a
-- JSON report.
--
-- The transactions run through 'execTx', which leaves the campaign alone: no
-- coverage is recorded, nothing reaches the corpus, and no test is falsified.
-- The point is to answer a question about the contract without changing what
-- the campaign does next.
executeSeq
  :: (MonadIO m, MonadReader Env m, MonadThrow m)
  => Bool -- ^ Whether to include the EVM trace of the replay
  -> VM Concrete -- ^ VM to replay from
  -> [Tx]
  -> m Text
executeSeq includeTrace vm0 txs = do
  dapp <- asks (.dapp)
  (outcomes, finalVm) <- go dapp vm0 (zip [1..] txs)
  let
    -- Summarise the sequence by its worst transaction. An assertion failure
    -- anywhere is what the caller is looking for, so it outranks a revert even
    -- when something reverted earlier; reverts are common enough in a random
    -- sequence that reporting one would bury it.
    notable =
      List.find ((== AssertionFailed) . (.status)) outcomes
        <|> List.find ((/= Completed) . (.status)) outcomes
    traceFields
      -- Traces are cleared before each transaction unless `allEvents` is set,
      -- so this is the trace of the last transaction alone in the usual case.
      -- 'showTraceTree' colours its output; whoever reads the report is not a
      -- terminal, so the escape codes are only tokens wasted.
      | includeTrace && not (null txs) =
          ["trace" .= stripAnsiEscapeCodes (showTraceTree dapp finalVm)]
      | otherwise = []
  pure $ LT.toStrict $ encodeToLazyText $ object $
    [ "status" .= maybe Completed (.status) notable
    , "transaction_count" .= length txs
    , "failed_tx_index" .= ((.index) <$> notable)
    , "final_block_number" .= show (forceLit finalVm.block.number)
    , "final_timestamp" .= show (forceLit finalVm.block.timestamp)
    , "transactions" .= outcomes
    ] ++ traceFields

  where
  -- Recursing rather than folding keeps the report in execution order, and
  -- hands back the final VM for the summary.
  go _ vm [] = pure ([], vm)
  go dapp vm ((index, tx):rest) = do
    (vmResult, vm') <- execTx vm tx
    call <- ppTx vm' False tx
    let
      result = getResult vmResult
      logs = extractEvents True dapp vm'
      outcome = TxOutcome { index
                          , call
                          , status = txStatus result logs
                          , result
                          , gasUsed = fromIntegral (vm'.burned - vm.burned)
                          , logs
                          }
    (outcomes, finalVm) <- go dapp vm' rest
    pure (outcome : outcomes, finalVm)

-- | Classify how a transaction ended, detecting assertion failures the same
-- way an assertion test does.
txStatus :: TxResult -> Events -> TxStatus
txStatus result logs
  | checkAssertionEvent logs || checkPanicEvent "1" logs = AssertionFailed
  | result `elem` [ReturnTrue, ReturnFalse, Stop] = Completed
  | otherwise = Reverted
