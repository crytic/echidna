{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Transaction where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.State.Strict (MonadState, gets)
import Data.HashMap.Strict qualified as M
import Data.Map (Map, toList)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import EVM hiding (value)
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
import Echidna.Types.Campaign (Campaign(..))

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
      in (addr,) <$> M.lookup metadata sigMap

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
mutateTx t@(Tx { call = SolCall c }) = do
  f <- oftenUsually skip mutate
  f c
  where mutate z = mutateAbiCall z >>= \c' -> pure $ t { call = SolCall c' }
        skip _ = pure t
mutateTx t = pure t

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: MonadState VM m => Tx -> m ()
setupTx (Tx NoCall _ r _ _ _ (t, b)) = fromEVM $ do
  state . pc .= 0
  state . stack .= mempty
  state . memory .= mempty
  block . timestamp %= (\x -> Lit (forceLit x + t))
  block . number += b
  loadContract r

setupTx (Tx c s r g gp v (t, b)) = fromEVM $ do
  result .= Nothing
  state . pc .= 0
  state . stack .= mempty
  state . memory .= mempty
  state . gas .= g
  tx . gasprice .= gp
  tx . origin .= s
  state . caller .= Lit (fromIntegral s)
  state . callvalue .= Lit v
  block . timestamp %= (\x -> Lit (forceLit x + t))
  block . number += b
  case c of
    SolCreate bc -> do
      env . contracts . at r .= Just (initialContract (InitCode bc mempty) & set balance v)
      loadContract r
      state . code .= RuntimeCode (ConcreteRuntimeCode bc)
    SolCall cd -> do
      incrementBalance
      loadContract r
      state . calldata .= ConcreteBuf (encode cd)
    SolCalldata cd -> do
      incrementBalance
      loadContract r
      state . calldata .= ConcreteBuf cd
  where
    incrementBalance = (env . contracts . ix r . balance) += v
    encode (n, vs) = abiCalldata
      (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs
