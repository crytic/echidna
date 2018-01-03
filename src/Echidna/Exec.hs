module Echidna.Exec (
    fuzz
  , solPredicate
) where

import Control.Lens ((^.), assign)
import Control.Monad (liftM2, replicateM)
import Control.Monad.State.Strict (execState, runState, State)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Vector (fromList)
import Hedgehog.Gen (sample)

import EVM (VM, VMResult(..), calldata, contract, loadContract, state)
import EVM.ABI (AbiType, AbiValue, abiCalldata)
import EVM.Concrete (Blob(..))
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Solidity (solidity)

import Echidna.ABI (genInteractions)

execCall :: ByteString -> State VM VMResult
execCall s = assign (state . calldata) (B s) >> exec

fuzz :: Int -- Call sequence length
     -> Int -- Number of iterations
     -> [(Text, [AbiType])] -- Type signatures to call
     -> VM -- Initial state
     -> (VM -> IO Bool) -- Predicate to fuzz for violations of
     -> IO (Maybe [ByteString]) -- Counterexample, if possible
fuzz l n ts v p = do
  calls <- replicateM n (replicateM l . sample $ genInteractions ts)
  results <- fmap (zip calls) $ mapM (p . (`execState` v) . mapM_ execCall) calls
  return $ listToMaybe [counter | (counter, passed) <- results, not passed]

-- Given a contract and a function call (assumed from that contract) return
-- an action loading that contract and calling that function if possible
solPredicate :: Text -> Text -> Text -> [AbiValue] -> IO (Maybe (State VM VMResult))
solPredicate name contents func args = do
  compiled <- solidity name contents
  case runState exec . vmForEthrunCreation <$> compiled of
    Just (VMSuccess _, vm) ->
      return (Just $ do loadContract (vm ^. state . contract)
                        execCall . abiCalldata func $ fromList args)
    _ -> return Nothing
