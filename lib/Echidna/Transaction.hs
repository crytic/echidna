{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Transaction where

import Optics.Core
import Optics.State.Operators

import Control.Monad (join, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State.Strict (MonadState, gets, modify', execState)
import Control.Monad.ST (RealWorld)
import Data.ByteString qualified as BS
import Data.Map (Map, toList)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V

import EVM (ceilDiv, initialContract, loadContract, resetState)
import EVM.ABI (abiValueType)
import EVM.FeeSchedule (FeeSchedule(..))
import EVM.Types hiding (Env, Gas, VMOpts(timestamp, gasprice))

import Echidna.ABI
import Echidna.Orphans.JSON ()
import Echidna.SourceMapping (lookupUsingCodehash)
import Echidna.SymExec.Symbolic (forceWord, forceAddr)
import Echidna.Types (fromEVM, Gas)
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Random
import Echidna.Types.Signature
  (SignatureMap, SolCall, ContractA)
import Echidna.Types.Tx
import Echidna.Types.World (World(..))
import Echidna.Types.Campaign

hasSelfdestructed :: VM Concrete s -> Addr -> Bool
hasSelfdestructed vm addr = LitAddr addr `elem` vm.tx.subState.selfdestructs

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
  :: (MonadIO m, MonadRandom m, MonadState WorkerState m, MonadReader Env m)
  => World
  -> Map (Expr EAddr) Contract
  -> m Tx
genTx world deployedContracts = do
  env <- ask
  let txConf = env.cfg.txConf
  genDict <- gets (.genDict)
  sigMap <- getSignatures world.highSignatureMap world.lowSignatureMap
  sender <- rElem' world.senders
  contractAList <- liftIO $ mapM (toContractA env sigMap) (toList deployedContracts)
  (dstAddr, dstAbis) <- rElem' $ Set.fromList $ catMaybes contractAList
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
    toContractA :: Env -> SignatureMap -> (Expr EAddr, Contract) -> IO (Maybe ContractA)
    toContractA env sigMap (addr, c) =
      fmap (forceAddr addr,) . snd <$> lookupUsingCodehash env.codehashMap c env.dapp sigMap

genDelay :: MonadRandom m => W256 -> Set W256 -> m W256
genDelay mv ds = do
  join $ oftenUsually fromDict randValue
  where randValue = fromIntegral <$> getRandomR (1 :: Integer, fromIntegral mv)
        fromDict = (`mod` (mv + 1)) <$> rElem' ds

genValue
  :: MonadRandom m
  => W256
  -> Set W256
  -> [FunctionSelector]
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
removeCallTx t = Tx NoCall t.src t.dst 0 0 0 t.delay

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx tx =
  let
    shrinkCall = case tx.call of
      SolCall sc -> SolCall <$> shrinkAbiCall sc
      _ -> pure tx.call
    lower 0 = pure 0
    lower x = (getRandomR (0 :: Integer, fromIntegral x)
                >>= (\r -> uniform [0, r]) . fromIntegral) -- try 0 quicker
    possibilities =
      [ do call' <- shrinkCall
           pure tx { call = call' }
      , do value' <- lower tx.value
           pure tx { Echidna.Types.Tx.value = value' }
      , do gasprice' <- lower tx.gasprice
           pure tx { Echidna.Types.Tx.gasprice = gasprice' }
      , do let (time, blocks) = tx.delay
           delay' <- join $ uniform [ (time,) <$> lower blocks
                                    , (,blocks) <$> lower time
                                    , (,) <$> lower time <*> lower blocks
                                    ]
           pure tx { delay = level delay' }
      ]
  in join $ usuallyRarely (join (uniform possibilities)) (pure $ removeCallTx tx)

mutateTx :: (MonadRandom m) => Tx -> m Tx
mutateTx tx@Tx{call = SolCall c} = do
  f <- oftenUsually skip mutate
  f c
  where mutate z = mutateAbiCall z >>= \c' -> pure tx { call = SolCall c' }
        skip _ = pure tx
mutateTx tx = pure tx

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: (MonadIO m, MonadState (VM Concrete RealWorld) m) => Tx -> m ()
setupTx tx@Tx{call = NoCall} = fromEVM $ do
  resetState
  modify' $ \vm -> vm
    { state = vm.state
    , block = advanceBlock vm.block tx.delay
    }

setupTx tx@Tx{call} = fromEVM $ do
  resetState
  modify' $ \vm -> vm
    { result = Nothing
    , state = vm.state
                 { gas = tx.gas
                 , caller = LitAddr (fromIntegral tx.src)
                 , callvalue = Lit tx.value
                 }
    , block = advanceBlock vm.block tx.delay
    , tx = vm.tx { gasprice = tx.gasprice, origin = LitAddr tx.src, subState = subState vm }
    }
  when isCreate $ do
    #env % #contracts % at (LitAddr tx.dst) .=
      Just (initialContract (InitCode calldata mempty) & set #balance (Lit tx.value))
    modify' $ execState $ loadContract (LitAddr tx.dst)
    #state % #code .= RuntimeCode (ConcreteRuntimeCode calldata)
  when isCall $ do
    incrementBalance
    modify' $ execState $ loadContract (LitAddr tx.dst)
    #state % #calldata .= ConcreteBuf calldata
  modify' $ \vm ->
    let intrinsicGas = txGasCost vm.block.schedule isCreate calldata
        burned = min intrinsicGas vm.state.gas
    in vm & #state % #gas %!~ subtract burned
          & #burned %!~ (+ burned)
  where
    incrementBalance = #env % #contracts % ix (LitAddr tx.dst) % #balance %= (\v -> Lit $ forceWord v + tx.value)
    encode (n, vs) = abiCalldata (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs
    isCall = case call of
      SolCall _ -> True
      SolCalldata _ -> True
      _ -> False
    isCreate = case call of
      SolCreate _ -> True
      _ -> False
    calldata = case call of
      SolCreate bc -> bc
      SolCall cd -> encode cd
      SolCalldata cd -> cd
    subState vm = let
        initialAccessedAddrs = Set.fromList $
            [LitAddr tx.src, LitAddr tx.dst, vm.block.coinbase]
          ++ fmap LitAddr [1..10] -- precompile addresses
        touched = if isCreate then [LitAddr tx.src] else [LitAddr tx.src, LitAddr tx.dst]
      in SubState mempty touched initialAccessedAddrs mempty mempty mempty

advanceBlock :: Block -> (W256, W256) -> Block
advanceBlock blk (t,b) =
  blk { timestamp = Lit (forceWord blk.timestamp + t)
      , number = Lit (forceWord blk.number + b) }

-- | Calculate transaction gas cost for Echidna Tx
-- Adapted from HEVM's txGasCost function
txGasCost :: FeeSchedule Gas -> Bool -> BS.ByteString -> Gas
txGasCost fs isCreate calldata = baseCost + zeroCost + nonZeroCost
  where
    zeroBytes = BS.count 0 calldata
    nonZeroBytes = BS.length calldata - zeroBytes
    baseCost = fs.g_transaction
      + (if isCreate then fs.g_txcreate + initcodeCost else 0)
    zeroCost = fs.g_txdatazero * fromIntegral zeroBytes
    nonZeroCost = fs.g_txdatanonzero * fromIntegral nonZeroBytes
    initcodeCost = fs.g_initcodeword * fromIntegral (ceilDiv (BS.length calldata) 32)
