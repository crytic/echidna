{-# LANGUAGE BangPatterns, DeriveGeneric, FlexibleContexts, KindSignatures, LambdaCase, StrictData #-}

module Echidna.Exec (
    VMState(..)
  , VMAction(..)
  , checkTest
  , checkBoolExpTest
  , checkRevertTest
  , checkTrueOrRevertTest
  , checkFalseOrRevertTest
  --, eCommand
  --, eCommandUsing
  , ePropertySeq
  --, ePropertyCheck
  --, ePropertyUsing
  , execCalls
  , execCallUsing
  , module Echidna.Internal.Runner
  , module Echidna.Internal.JsonRunner
  ) where

import Control.Lens               ((&), (^.), (.=), (?~))
import Control.Monad              (foldM)
import Control.Monad.Catch        (MonadCatch)
import Control.Monad.State.Strict (MonadState, evalState, execState, get, put)
import Control.Monad.Reader       (MonadReader, runReaderT, ask)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Data.List                  (intercalate)
import Data.Text                  (Text)
import Data.Typeable              (Typeable)
import Data.Vector                (fromList)

import Hedgehog
import Hedgehog.Gen               (sequential, sample, list, print)
import Hedgehog.Internal.State    (Action(..))
import Hedgehog.Internal.Property (PropertyConfig(..), mapConfig)
import Hedgehog.Range             (linear)

import EVM
import EVM.ABI      (AbiValue(..), abiCalldata, abiValueType, encodeAbiValue)
import EVM.Concrete (Blob(..))
import EVM.Exec     (exec)

import Echidna.ABI (SolCall, SolSignature, displayAbiCall, encodeSig, genInteractions, genTransactions)
import Echidna.Config (Config(..), testLimit, range, shrinkLimit)
import Echidna.Internal.Runner
import Echidna.Internal.JsonRunner
import Echidna.Property (PropertyType(..))

-------------------------------------------------------------------
-- Fuzzing and Hedgehog Init

execCalls :: [SolCall] -> VM -> VM
execCalls cs ivm = foldr f (execState exec ivm) cs
                   where f c vm = execState (execCallUsing c exec) vm 
                                   --Just x -> execState (execCallUsing c (return x)) vm 

execCall :: MonadState VM m => SolCall -> m VMResult
execCall c = execCallUsing c exec

execCallUsing :: MonadState VM m => SolCall -> m VMResult -> m VMResult
execCallUsing (t,vs) m = do og <- get
                            cleanUpAfterTransaction
                            state . calldata .= encodeSolCall t vs
                            m >>= \case x@VMFailure{} -> put (og & result ?~ x) >> return x
                                        x@VMSuccess{} -> return x
  
encodeSolCall :: Text -> [AbiValue] -> Blob        
encodeSolCall t vs =  B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs

cleanUpAfterTransaction :: MonadState VM m => m ()
cleanUpAfterTransaction = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]

ePropertySeq :: (VM -> Bool) -> [SolSignature] -> VM -> Config -> IO ()
ePropertySeq p ts ivm c = do cs <- sample $ runReaderT (genTransactions 10 ts) c
                             let vm_ = execCalls cs ivm
                             Prelude.print cs
                             Prelude.print (vm_ ^. result)
                             Prelude.print $ p vm_
                             --if (p vm_) then
                             --   Prelude.print "true!"
                             --else
                             --   Prelude.print "false!"
                           --ePropertySeq p ts vm c 

--ePropertyCheck :: (MonadGen m, MonadState VM m) =>
--                        (VM -> Bool) -> [SolSignature] -> VM -> Config -> m ()
--ePropertyCheck p ts vm c = evalState $ ePropertySeq p ts vm c
                           --  evalState r 


{-
execCallUsing m (t,vs) = do og <- get
                            cleanUp 
                            state . calldata .= cd
                            m >>= \case x@VMFailure{} -> put (og & result ?~ x) >> return x
                                        x@VMSuccess{} -> return x
  where cd = B . abiCalldata (encodeSig t $ abiValueType <$> vs) $ fromList vs
        cleanUp = sequence_ [result .= Nothing, state . pc .= 0, state . stack .= mempty]
-}

checkTest :: PropertyType -> VM -> Text -> Bool
checkTest ShouldReturnTrue             = checkBoolExpTest True
checkTest ShouldReturnFalse            = checkBoolExpTest False
checkTest ShouldRevert                 = checkRevertTest
checkTest ShouldReturnFalseRevert      = checkFalseOrRevertTest

checkBoolExpTest :: Bool -> VM -> Text -> Bool
checkBoolExpTest b v t = case evalState (execCall (t, [])) v of
  VMSuccess (B s) -> s == encodeAbiValue (AbiBool b)
  _               -> False

checkRevertTest :: VM -> Text -> Bool
checkRevertTest v t = case evalState (execCall (t, [])) v of
  (VMFailure Revert) -> True
  _                  -> False

checkTrueOrRevertTest :: VM -> Text -> Bool
checkTrueOrRevertTest v t = case evalState (execCall (t, [])) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool True)
  (VMFailure Revert) -> True
  _                  -> False

checkFalseOrRevertTest :: VM -> Text -> Bool
checkFalseOrRevertTest v t = case evalState (execCall (t, [])) v of
  (VMSuccess (B s))  -> s == encodeAbiValue (AbiBool False)
  (VMFailure Revert) -> True
  _                  -> False


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

{-
eCommandUsing :: (MonadGen n, MonadTest m, Typeable a)
              => n SolCall
              -> (VMAction Concrete -> m a)
              -> (VM -> Bool)
              -> Command n m VMState
eCommandUsing gen ex p = Command (\_ -> pure $ Call <$> gen) ex
  [ Ensure $ \_ (VMState v) _ _ -> assert $ p v
  , Update $ \(VMState v) (Call c) _ -> VMState $ execState (execCall c) v
  ]
  

eCommand :: (MonadGen n, MonadTest m) => n SolCall -> (VM -> Bool) -> Command n m VMState
eCommand = flip eCommandUsing (\ _ -> pure ())

configProperty :: Config -> PropertyConfig -> PropertyConfig
configProperty config x = x { propertyTestLimit   = config ^. testLimit
                            , propertyShrinkLimit = config ^. shrinkLimit
                            }

ePropertyUsing :: (MonadCatch m, MonadTest m, MonadReader Config n)
             => [Command Gen m VMState]
             -> (m () -> PropertyT IO ())
             -> VM             
             -> n Property
ePropertyUsing cs f v = do
  config <- ask
  return $ mapConfig (configProperty config) . property $
    f . executeSequential (VMState v) =<< forAllWith printCallSeq
    (sequential (linear 1 (config ^. range)) (VMState v) cs)
  where printCallSeq = ("Call sequence: " ++) . intercalate "\n               " .
          map showCall . sequentialActions
        showCall (Action i _ _ _ _ _) = show i ++ ";"


ePropertySeq :: (MonadReader Config m)
             => (VM -> Bool)   -- Predicate to fuzz for violations of
             -> [SolSignature] -- Type signatures to fuzz
             -> VM             -- Initial state
             -> m Property
ePropertySeq p ts vm = ask >>= \c -> ePropertyUsing [eCommand (runReaderT (genInteractions ts) c) p] id vm

-}
