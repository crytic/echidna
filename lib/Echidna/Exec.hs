{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase #-}

module Echidna.Exec (
    checkETest
  , cleanUp
  , eCommand
  , ePropertySeq
  , execCall
  , fuzz
  , module Echidna.Internal.Runner
  ) where

import Control.Lens               ((^.), (.=), use)
import Control.Monad              (forM_, replicateM)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.State.Strict (MonadState, evalState, execState, get, runState)
import Data.List                  (intercalate)
import Data.Maybe                 (listToMaybe)
import Data.Set                   (Set, singleton)
import Data.Text                  (Text)
import Data.Vector                (fromList)

import qualified Control.Monad.State.Strict as S

import Hedgehog
import Hedgehog.Gen               (sample, sequential)
import Hedgehog.Internal.State    (Action(..))
import Hedgehog.Internal.Property (PropertyConfig(..), mapConfig)
import Hedgehog.Range             (linear)

import EVM          (VM, VMResult(..), calldata, exec1, pc, result, stack, state)
import EVM.ABI      (AbiValue(..), abiCalldata, abiValueType, encodeAbiValue)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec)
import EVM.UnitTest (OpLocation, currentOpLocation)

import Echidna.ABI (SolCall, SolSignature, displayAbiCall, encodeSig, genInteractions)
import Echidna.Internal.Runner

execCall :: MonadState VM m => SolCall -> m VMResult
execCall (t,vs) = cleanUp >> (state . calldata .= cd >> exec) where
  cd = B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs

execWithCoverage :: (MonadState VM m, MonadWriter (Set OpLocation) m) => m VMResult
execWithCoverage = use result >>= \case
  Just x -> return x
  _      -> do tell . singleton . currentOpLocation =<< get
               S.state (runState exec1)
               exec

-- FIXME(jp): this + execCall should be special cases of /something/
execCallWithCoverage :: (MonadState VM m, MonadWriter (Set OpLocation) m) => SolCall -> m VMResult
execCallWithCoverage (t,vs) = cleanUp >> (state . calldata .= cd >> execWithCoverage) where
  cd = B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs

fuzz :: MonadIO m
     => Int                 -- Call sequence length
     -> Int                 -- Number of iterations
     -> [SolSignature]      -- Type signatures to call
     -> VM                  -- Initial state
     -> (VM -> m Bool)      -- Predicate to fuzz for violations of
     -> m (Maybe [SolCall]) -- Call sequence to violate predicate (if found)
fuzz l n ts v p = do
  callseqs <- replicateM n (replicateM l . sample $ genInteractions ts)
  results <- zip callseqs <$> mapM run callseqs
  return $ listToMaybe [cs | (cs, passed) <- results, not passed]
    where run cs = p $ execState (forM_ cs execCall) v

cleanUp :: MonadState VM m => m ()
cleanUp = sequence_ [ result        .= Nothing
                    , state . pc    .= 0
                    , state . stack .= mempty
                    ]

checkETest :: VM -> Text -> Bool
checkETest v t = case evalState (execCall (t, [])) v of
  VMSuccess (B s) -> s == encodeAbiValue (AbiBool True)
  _               -> False

newtype VMState (v :: * -> *) =
  VMState VM

instance Show (VMState v) where
  show (VMState v) = "EVM state, current result: " ++ show (v ^. result)

newtype VMAction (v :: * -> *) = 
  Call SolCall

instance Show (VMAction v) where
  show (Call c) = displayAbiCall c

instance HTraversable VMAction where
  htraverse _ (Call b) = pure $ Call b

eCommand :: (MonadGen n, MonadTest m) => [SolSignature] -> (VM -> Bool) -> Command n m VMState
eCommand ts p = Command (\_ -> pure $ Call <$> genInteractions ts)
                        (\_ -> pure ())
                        [ Ensure $ \_ (VMState v) _ _ -> assert $ p v
                        , Update $ \(VMState v) (Call c) _ ->
                                       VMState $ execState (execCall c) v
                        ]

ePropertySeq :: VM             -- Initial state
             -> [SolSignature] -- Type signatures to fuzz
             -> (VM -> Bool)   -- Predicate to fuzz for violations of
             -> Int            -- Max actions to execute
             -> Property
ePropertySeq v ts p n = mapConfig (\x -> x {propertyTestLimit = 10000}) . property $
  executeSequential (VMState v) =<< forAllWith printCallSeq
  (sequential (linear 1 n) (VMState v) [eCommand ts p]) where
    printCallSeq = ("Call sequence: " ++) . intercalate "\n               " .
      map showCall . reverse . sequentialActions
    showCall (Action i _ _ _ _ _) = show i ++ ";"

-- Should work, but missing instance MonadBaseControl b m => MonadBaseControl b (PropertyT m)
-- ePropertyPar :: VM                  -- Initial state
             -- -> [(Text, [AbiType])] -- Type signatures to fuzz
             -- -> (VM -> Bool)        -- Predicate to fuzz for violations of
             -- -> Int                 -- Max size
             -- -> Int                 -- Max post-prefix size
             -- -> Property
-- ePropertyPar v ts p n m = withRetries 10 . property $ executeParallel (Current v) =<<
--   forAll (parallel (linear 1 n) (linear 1 m) (Current v) [eCommand v ts p])
