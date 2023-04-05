{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Transaction where

import Optics.Core
import Optics.State
import Optics.State.Operators

import Control.Monad (join)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.State.Strict (MonadState, gets, modify')
import Data.HashMap.Strict qualified as M
import Data.Map (Map, toList)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import EVM
  ( bytecode, initialContract, loadContract, Block(..), Contract, ContractCode(..)
  , FrameState(..), RuntimeCode(..), SubState(..), TxState(..), VM(..) )
import EVM.ABI (abiValueType)
import EVM.Types (Expr(ConcreteBuf, Lit), Addr, W256, FunctionSelector)

import Echidna.ABI
import Echidna.Types.Random
import Echidna.Orphans.JSON ()
import Echidna.Types (fromEVM)
import Echidna.Types.Buffer (forceBuf, forceLit)
import Echidna.Types.Signature (SignatureMap, SolCall, ContractA, MetadataCache, lookupBytecodeMetadata)
import Echidna.Types.Tx
import Echidna.Types.Tx qualified as Tx
import Echidna.Types.World (World(..))
import Echidna.Types.Campaign (Campaign(..))

hasSelfdestructed :: VM -> Addr -> Bool
hasSelfdestructed vm addr = addr `elem` vm.tx.substate.selfdestructs

-- | If half a tuple is zero, make both halves zero. Useful for generating
-- delays, since block number only goes up with timestamp
level :: (Num a, Eq a) => (a, a) -> (a, a)
level (x, y) | x == 0 || y == 0 = (0, 0)
level x = x

getSignatures :: MonadRandom m => SignatureMap -> Maybe SignatureMap -> m SignatureMap
getSignatures hmm Nothing = return hmm
getSignatures hmm (Just lmm) = usuallyVeryRarely hmm lmm -- once in a while, this will use the low-priority signature for the input generation

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTx :: (MonadRandom m, MonadState Campaign m)
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
      in (addr,) <$> M.lookup metadata sigMap

genDelay :: MonadRandom m => W256 -> Set W256 -> m W256
genDelay mv ds = do
  join $ oftenUsually fromDict randValue
  where randValue = fromIntegral <$> getRandomR (1 :: Integer, fromIntegral mv)
        fromDict = (`mod` (mv + 1)) <$> rElem' ds

genValue :: MonadRandom m => W256 -> Set W256 -> [FunctionSelector] -> SolCall -> m W256
genValue mv ds ps sc =
  if sig `elem` ps then
    join $ oftenUsually fromDict randValue
  else
    -- once in a while, this will generate value in a non-payable function
    join $ usuallyVeryRarely (pure 0) randValue
  where randValue = fromIntegral <$> getRandomR (0 :: Integer, fromIntegral mv)
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
shrinkTx tx =
  join $ usuallyRarely (join (uniform possibilities)) (pure $ removeCallTx tx)
  where
  shrinkCall = case tx.call of
    SolCall sc -> SolCall <$> shrinkAbiCall sc
    _ -> pure tx.call
  lower 0 = pure 0
  lower x = getRandomR (0 :: Integer, fromIntegral x)
              >>= (\r -> uniform [0, r]) . fromIntegral  -- try 0 quicker
  possibilities =
    [ do call <- shrinkCall
         pure tx { call }
    , do value <- lower tx.value
         pure tx { Tx.value }
    , do gasprice <- lower tx.gasprice
         pure tx { Tx.gasprice }
    , do let (time, blocks) = tx.delay
         delay <- level <$> ((,) <$> lower time <*> lower blocks)
         pure tx { delay }
    ]

mutateTx :: (MonadRandom m) => Tx -> m Tx
mutateTx t@(Tx { call = SolCall c }) = do
  f <- oftenUsually skip mutate
  f c
  where mutate z = mutateAbiCall z >>= \c' -> pure $ t { call = SolCall c' }
        skip _ = pure t
mutateTx t = pure t

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively,
-- this just brings 'Transaction's \"on-chain\".
setupTx :: MonadState VM m => Tx -> m ()
setupTx tx@Tx{ call = NoCall } = fromEVM $ do
  modify' $ \vm -> vm
    { state = resetState vm.state
    , block = advanceBlock tx.delay vm.block
    }
  loadContract tx.src

setupTx tx@Tx{ call } = fromEVM $ do
  modify' $ \vm -> vm
    { result = Nothing
    , state = (resetState vm.state)
                { gas = tx.gas
                , caller = Lit (fromIntegral tx.src)
                , callvalue = Lit tx.value
                }
    , tx = vm.tx { gasprice = tx.gasprice, origin = tx.src }
    , block = advanceBlock tx.delay vm.block
    }
  case call of
    SolCreate bc -> do
      assign (#env % #contracts % at tx.dst)
             (Just $ initialContract (InitCode bc mempty) & set #balance tx.value)
      loadContract tx.dst
      #state % #code .= RuntimeCode (ConcreteRuntimeCode bc)
    SolCall cd -> do
      incrementBalance
      loadContract tx.dst
      #state % #calldata .= ConcreteBuf (encode cd)
    SolCalldata cd -> do
      incrementBalance
      loadContract tx.dst
      #state % #calldata .= ConcreteBuf cd
  where
  incrementBalance = #env % #contracts % ix tx.dst % #balance %= (+ tx.value)
  encode (n, vs) = abiCalldata
    (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs

resetState :: FrameState -> FrameState
resetState state = state { pc = 0, stack = mempty, memory = mempty }

advanceBlock :: (W256, W256) -> Block -> Block
advanceBlock (t,b) block =
  block { timestamp = Lit (forceLit block.timestamp + t)
        , number = block.number + b }
