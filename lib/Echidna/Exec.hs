{-# LANGUAGE FlexibleContexts, KindSignatures, LambdaCase #-}

module Echidna.Exec (
    Coverage
  , checkETest
  , eCommand
  , eCommandCoverage
  , ePropertySeq
  , ePropertySeqCoverage
  , execCall
  , execCallCoverage
  , fuzz
  , module Echidna.Internal.Runner
  ) where

import Control.Lens                ((^.), (.=), use)
import Control.Monad               (forM_, replicateM)
import Control.Monad.Catch         (MonadCatch)
import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.State.Strict  (MonadState, StateT, evalState, evalStateT, execState, get, runState)
import Control.Monad.Writer.Class  (MonadWriter, listen, tell)
import Control.Monad.Writer.Strict (WriterT, (<>), runWriterT)
import Data.IORef                  (IORef, modifyIORef)
import Data.List                   (intercalate)
import Data.Maybe                  (listToMaybe)
import Data.MultiSet               (MultiSet, singleton)
import Data.Text                   (Text)
import Data.Typeable               (Typeable)
import Data.Vector                 (fromList)

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

execCallUsing :: MonadState VM m => m VMResult -> SolCall -> m VMResult
execCallUsing m (t,vs) = cleanUp >> (state . calldata .= cd >> m) where
  cd = B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs
  cleanUp = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]

execCall :: MonadState VM m => SolCall -> m VMResult
execCall = execCallUsing exec

type Coverage = MultiSet OpLocation

execCallCoverage :: (MonadState VM m, MonadWriter Coverage m) => SolCall -> m VMResult
execCallCoverage = execCallUsing go where
  go = use result >>= \case
    Just x -> return x
    _      -> do tell . singleton . currentOpLocation =<< get
                 S.state (runState exec1)
                 go

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

eCommandUsing :: (MonadGen n, MonadTest m, Typeable a)
              => (VMAction Concrete -> m a)
              -> (a -> VM -> Bool)
              -> [SolSignature]
              -> Command n m VMState
eCommandUsing o p ts = Command (\_ -> pure $ Call <$> genInteractions ts) o 
  [ Ensure $ \_ (VMState v) _ x -> assert $ p x v
  , Update $ \(VMState v) (Call c) _ -> VMState $ execState (execCall c) v
  ]

eCommand :: (MonadGen n, MonadTest m) => (VM -> Bool) -> [SolSignature] -> Command n m VMState
eCommand = eCommandUsing (\_ -> pure ()) . const

eCommandCoverage :: (MonadGen n, MonadTest m, MonadState VM m, MonadWriter Coverage m)
                 => (MultiSet OpLocation -> VM -> Bool) -> [SolSignature] -> Command n m VMState
eCommandCoverage = eCommandUsing $ \(Call c) -> execCallCoverage c >> snd <$> listen (pure ())

ePropertyUsing :: (MonadCatch m, MonadTest m)
               => Command Gen m VMState
               -> (m () -> PropertyT IO ())
               -> VM
               -> Int
               -> Property
ePropertyUsing c f v n = mapConfig (\x -> x {propertyTestLimit = 10000}) . property $ 
  f . executeSequential (VMState v) =<< forAllWith printCallSeq
  (sequential (linear 1 n) (VMState v) [c]) where
    printCallSeq = ("Call sequence: " ++) . intercalate "\n               " .
      map showCall . reverse . sequentialActions
    showCall (Action i _ _ _ _ _) = show i ++ ";"

ePropertySeq :: (VM -> Bool)   -- Predicate to fuzz for violations of
             -> [SolSignature] -- Type signatures to fuzz
             -> VM             -- Initial state
             -> Int            -- Max actions to execute
             -> Property
ePropertySeq p ts = ePropertyUsing (eCommand p ts) id 

ePropertySeqCoverage :: IORef Coverage
                     -> (Coverage -> VM -> Bool)
                     -> [SolSignature]
                     -> VM
                     -> Int
                     -> Property
ePropertySeqCoverage r p ts v = ePropertyUsing (eCommandCoverage p ts) writeCoverage v where
  writeCoverage :: MonadIO m => WriterT Coverage (StateT VM m) a -> m a
  writeCoverage m = flip evalStateT v $ do (a, w) <- runWriterT m
                                           liftIO $ modifyIORef r (<> w)
                                           return a

-- Should work, but missing instance MonadBaseControl b m => MonadBaseControl b (PropertyT m)
-- ePropertyPar :: VM                  -- Initial state
             -- -> [(Text, [AbiType])] -- Type signatures to fuzz
             -- -> (VM -> Bool)        -- Predicate to fuzz for violations of
             -- -> Int                 -- Max size
             -- -> Int                 -- Max post-prefix size
             -- -> Property
-- ePropertyPar v ts p n m = withRetries 10 . property $ executeParallel (Current v) =<<
--   forAll (parallel (linear 1 n) (linear 1 m) (Current v) [eCommand v ts p])
