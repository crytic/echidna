{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Transaction where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.State.Strict (MonadState, gets, modify')
import Data.Map (Map, toList)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import EVM hiding (resetState, tx, value)
import EVM.ABI (abiValueType)
import EVM.Types (Expr(ConcreteBuf, Lit), Addr, W256)

import Echidna.ABI
import Echidna.Types.Random
import Echidna.Orphans.JSON ()
import Echidna.Types (fromEVM)
import Echidna.Types.Buffer (forceBuf, forceLit)
import Echidna.Types.Signature (SignatureMap, SolCall, ContractA, FunctionHash, MetadataCache, lookupBytecodeMetadata)
import Echidna.Types.Tx
import Echidna.Types.World (World(..))
import Echidna.Types.Campaign

hasSelfdestructed :: VM -> Addr -> Bool
hasSelfdestructed vm addr = addr `elem` vm._tx._substate._selfdestructs

-- | If half a tuple is zero, make both halves zero. Useful for generating
-- delays, since block number only goes up with timestamp
level :: (Num a, Eq a) => (a, a) -> (a, a)
level (x, y) | x == 0 || y == 0 = (0, 0)
level x = x

getSignatures
  :: MonadRandom m
  => SignatureMap
  -> Maybe SignatureMap
  -> m SignatureMap
getSignatures hmm Nothing = pure hmm
getSignatures hmm (Just lmm) =
  -- once in a while, this will use the low-priority signature for the input generation
  usuallyVeryRarely hmm lmm

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTx
  :: (MonadRandom m, MonadState Campaign m)
  => MetadataCache
  -> World
  -> TxConf
  -> Map Addr Contract
  -> m Tx
genTx memo world txConf deployedContracts = do
  genDict <- gets (.genDict)
  sigMap <- getSignatures world.highSignatureMap world.lowSignatureMap
  sender <- rElem' world.senders
  (dstAddr, dstAbis) <- rElem' $ Set.fromList $
    mapMaybe (toContractA sigMap) (toList deployedContracts)
  solCall <- genInteractionsM genDict dstAbis
  value <- genValue txConf.maxValue genDict.dictValues world.payableSigs solCall
  ts <- (,) <$> genDelay txConf.maxTimeDelay genDict.dictValues
            <*> genDelay txConf.maxBlockDelay genDict.dictValues
  pure $ Tx { call = SolCall solCall
            , src = sender
            , dst = dstAddr
            , gas = txConf.txGas
            , gasprice = txConf.maxGasprice
            , value = value
            , delay = level ts
            }
  where
    toContractA :: SignatureMap -> (Addr, Contract) -> Maybe ContractA
    toContractA sigMap (addr, c) =
      let bc = forceBuf $ c ^. bytecode
          metadata = lookupBytecodeMetadata memo bc
      in (addr,) <$> Map.lookup metadata sigMap

genDelay :: MonadRandom m => W256 -> Set W256 -> m W256
genDelay mv ds = do
  join $ oftenUsually fromDict randValue
  where randValue = fromIntegral <$> getRandomR (1 :: Integer, fromIntegral mv)
        fromDict = (`mod` (mv + 1)) <$> rElem' ds

genValue
  :: MonadRandom m
  => W256
  -> Set W256
  -> [FunctionHash]
  -> SolCall
  -> m W256
genValue mv ds ps sc =
  if sig `elem` ps then
    join $ oftenUsually fromDict randValue
  else
    -- once in a while, this will generate value in a non-payable function
    join $ usuallyVeryRarely (pure 0) randValue
  where
    randValue = fromIntegral <$> getRandomR (0 :: Integer, fromIntegral mv)
    sig = (hashSig . encodeSig . signatureCall) sc
    fromDict = (`mod` (mv + 1)) <$> rElem' ds

-- | Check if a 'Transaction' is as \"small\" (simple) as possible (using ad-hoc heuristics).
canShrinkTx :: Tx -> Bool
canShrinkTx Tx { call, gasprice = 0, value = 0, delay = (0, 0) } =
  case call of
    SolCall (_, l) -> any canShrinkAbiValue l
    _ -> False
canShrinkTx _ = True

removeCallTx :: Tx -> Tx
removeCallTx t = Tx NoCall 0 t.src 0 0 0 t.delay

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx tx' =
  let
    shrinkCall = case tx'.call of
      SolCall sc -> SolCall <$> shrinkAbiCall sc
      _ -> pure tx'.call
    lower 0 = pure 0
    lower x = (getRandomR (0 :: Integer, fromIntegral x)
                >>= (\r -> uniform [0, r]) . fromIntegral) -- try 0 quicker
    possibilities =
      [ do call' <- shrinkCall
           pure tx' { call = call' }
      , do value' <- lower tx'.value
           pure tx' { Echidna.Types.Tx.value = value' }
      , do gasprice' <- lower tx'.gasprice
           pure tx' { Echidna.Types.Tx.gasprice = gasprice' }
      , do let (time, blocks) = tx'.delay
           delay' <- level <$> ((,) <$> lower time <*> lower blocks)
           pure tx' { delay = delay' }
      ]
  in join $ usuallyRarely (join (uniform possibilities)) (pure $ removeCallTx tx')

mutateTx :: (MonadRandom m) => Tx -> m Tx
mutateTx tx@Tx{call = SolCall c} = do
  f <- oftenUsually skip mutate
  f c
  where mutate z = mutateAbiCall z >>= \c' -> pure tx { call = SolCall c' }
        skip _ = pure tx
mutateTx tx = pure tx

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: MonadState VM m => Tx -> m ()
setupTx tx@Tx{call = NoCall} = fromEVM $ do
  modify' $ \vm -> vm
    { _state = resetState vm._state
    , _block = advanceBlock vm._block tx.delay
    }
  loadContract tx.dst

setupTx tx@Tx{call} = fromEVM $ do
  modify' $ \vm -> vm
    { _result = Nothing
    , _state = (resetState vm._state)
                 { _gas = tx.gas
                 , _caller = Lit (fromIntegral tx.src)
                 , _callvalue = Lit tx.value
                 }
    , _block = advanceBlock vm._block tx.delay
    , _tx = vm._tx { _gasprice = tx.gasprice, _origin = tx.src }
    }
  case call of
    SolCreate bc -> do
      env . contracts . at tx.dst .= Just (initialContract (InitCode bc mempty) & set balance tx.value)
      loadContract tx.dst
      state . code .= RuntimeCode (ConcreteRuntimeCode bc)
    SolCall cd -> do
      incrementBalance
      loadContract tx.dst
      state . calldata .= ConcreteBuf (encode cd)
    SolCalldata cd -> do
      incrementBalance
      loadContract tx.dst
      state . calldata .= ConcreteBuf cd
  where
    incrementBalance = env . contracts . ix tx.dst . balance += tx.value
    encode (n, vs) = abiCalldata (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs

resetState :: FrameState -> FrameState
resetState s = s { _pc = 0, _stack = mempty, _memory = mempty }

advanceBlock :: Block -> (W256, W256) -> Block
advanceBlock blk (t,b) =
  blk { _timestamp = Lit (forceLit blk._timestamp + t)
      , _number = blk._number + b }
