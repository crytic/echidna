{-# LANGUAGE KindSignatures #-}

module Echidna.Exec (
    eCommand
  , ePropertySeq
  , fuzz
  , solPredicate
) where

import Control.Lens ((^.), assign)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (evalState, execState, runState, State)
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Vector (fromList)
import Hedgehog
import Hedgehog.Gen (sample, sequential)
import Hedgehog.Range (linear)

import EVM (VM, VMResult(..), calldata, contract, loadContract, result, state)
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
  results <- zip calls <$> mapM (p . (`execState` v) . mapM_ execCall) calls
  return $ listToMaybe [input | (input, worked) <- results, not worked]

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

newtype VMState (v :: * -> *) =
  Current VM

instance Show (VMState v) where
  show (Current v) = "EVM state, current result: " ++ show (v ^. result)

newtype VMAction (v :: * -> *) = 
  Call ByteString

instance Show (VMAction v) where
  show (Call b) = "EVM call with data: " ++ show b

instance HTraversable VMAction where
  htraverse _ (Call b) = pure $ Call b

eCommand :: (MonadGen n, MonadTest m) => VM -> [(Text, [AbiType])] -> (VM -> Bool) -> Command n m VMState
eCommand v ts p = Command (const . Just . fmap Call $ genInteractions ts)
                          (\(Call b) -> pure $ evalState (execCall b) v)
                          [Ensure $ \_ (Current s) _ _ -> assert $ p s]

ePropertySeq :: VM                  -- Initial state
             -> [(Text, [AbiType])] -- Type signatures to fuzz
             -> (VM -> Bool)        -- Predicate to fuzz for violations of
             -> Int                 -- Max actions to execute
             -> Property
ePropertySeq v ts p n = property $ executeSequential (Current v) =<<
  forAll (sequential (linear 1 n) (Current v) [eCommand v ts p])

-- Should work, but missing instance MonadBaseControl b m => MonadBaseControl b (PropertyT m)
-- ePropertyPar :: VM                  -- Initial state
             -- -> [(Text, [AbiType])] -- Type signatures to fuzz
             -- -> (VM -> Bool)        -- Predicate to fuzz for violations of
             -- -> Int                 -- Max size
             -- -> Int                 -- Max post-prefix size
             -- -> Property
-- ePropertyPar v ts p n m = withRetries 10 . property $ executeParallel (Current v) =<<
--   forAll (parallel (linear 1 n) (linear 1 m) (Current v) [eCommand v ts p])
