{-# LANGUAGE ViewPatterns #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (join, liftM2)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, runState, get, put)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Has (Has(..))
import Data.HashMap.Strict qualified as M
import Data.Map (Map, toList)
import Data.Maybe (catMaybes)
import Data.Vector qualified as V
import EVM hiding (value)
import EVM.ABI (abiValueType)
import EVM.Symbolic (litWord, litAddr)
import EVM.Types (Addr, Buffer(..), Word(..), w256, w256lit, SymWord)

import Echidna.ABI
import Echidna.Types.Random
import Echidna.Orphans.JSON ()
import Echidna.Types.Buffer (viewBuffer)
import Echidna.Types.Signature (SignatureMap, SolCall, ContractA, FunctionHash, BytecodeMemo, lookupBytecodeMetadata)
import Echidna.Types.Tx
import Echidna.Types.World (World(..))

hasSelfdestructed :: (MonadState y m, Has VM y) => Addr -> m Bool
hasSelfdestructed a = do
  sd <- use $ hasLens . tx . substate . selfdestructs
  return $ a `elem` sd

-- | If half a tuple is zero, make both halves zero. Useful for generating delays, since block number
-- only goes up with timestamp
level :: (Num a, Eq a) => (a, a) -> (a, a)
level (elemOf each 0 -> True) = (0,0)
level x                       = x

getSignatures :: MonadRandom m => SignatureMap -> Maybe SignatureMap -> m SignatureMap
getSignatures hmm Nothing = return hmm
getSignatures hmm (Just lmm) = usuallyVeryRarely hmm lmm -- once in a while, this will use the low-priority signature for the input generation

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTxM :: (MonadRandom m, MonadReader x m, MonadState y m, Has TxConf x, Has World x, Has GenDict y)
  => BytecodeMemo
  -> Map Addr Contract
  -> m Tx
genTxM memo m = do
  TxConf _ g gp t b mv <- view hasLens
  World ss hmm lmm ps _ <- view hasLens
  genDict <- use hasLens
  mm <- getSignatures hmm lmm
  let ns = dictValues genDict
  s' <- rElem ss
  r' <- rElem $ NE.fromList . catMaybes $ toContractA mm <$> toList m
  c' <- genInteractionsM genDict (snd r')
  v' <- genValue mv ns ps c'
  t' <- (,) <$> genDelay t ns <*> genDelay b ns
  pure $ Tx (SolCall c') s' (fst r') g gp v' (level t')
  where
    toContractA :: SignatureMap -> (Addr, Contract) -> Maybe ContractA
    toContractA mm (addr, c) = do
      bc <- viewBuffer $ c ^. bytecode
      let metadata = lookupBytecodeMetadata memo bc
      (addr,) <$> M.lookup metadata mm

genDelay :: MonadRandom m => Word -> [Integer] -> m Word
genDelay mv ds = do
  let ds' = map (`mod` (fromIntegral mv + 1)) ds
  g <- oftenUsually randValue $ rElem (0 NE.:| ds')
  w256 . fromIntegral <$> g
  where randValue = getRandomR (1 :: Integer, fromIntegral mv)

genValue :: MonadRandom m => Word -> [Integer] -> [FunctionHash] -> SolCall -> m Word
genValue mv ds ps sc =
  if sig `elem` ps then do
    let ds' = map (`mod` (fromIntegral mv + 1)) ds
    g <- oftenUsually randValue $ rElem (0 NE.:| ds')
    fromIntegral <$> g
  else do
    g <- usuallyVeryRarely (pure 0) randValue -- once in a while, this will generate value in a non-payable function
    fromIntegral <$> g
  where randValue = getRandomR (0 :: Integer, fromIntegral mv)
        sig = (hashSig . encodeSig . signatureCall) sc

-- | Check if a 'Transaction' is as \"small\" (simple) as possible (using ad-hoc heuristics).
canShrinkTx :: Tx -> Bool
canShrinkTx (Tx solcall _ _ _ 0 0 (0, 0)) =
  case solcall of
    SolCall (_, l) -> any canShrinkAbiValue l
    _ -> False
canShrinkTx _ = True

removeCallTx :: Tx -> Tx
removeCallTx (Tx _ _ r _ _ _ d) = Tx NoCall 0 r 0 0 0 d

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx tx'@(Tx c _ _ _ gp (C _ v) (C _ t, C _ b)) = let
  c' = case c of
         SolCall sc -> SolCall <$> shrinkAbiCall sc
         _ -> pure c
  lower 0 = pure $ w256 0
  lower x = (getRandomR (0 :: Integer, fromIntegral x)
              >>= (\r -> uniform [0, r]) . w256 . fromIntegral)  -- try 0 quicker
  possibilities =
    [ set call      <$> c'
    , set value     <$> lower v
    , set gasprice' <$> lower gp
    , set delay     <$> fmap level (liftM2 (,) (lower t) (lower b))
    ]
  in join $ usuallyRarely (join (uniform possibilities) <*> pure tx') (pure $ removeCallTx tx')

mutateTx :: (MonadRandom m) => Tx -> m Tx
mutateTx t@(Tx (SolCall c) _ _ _ _ _ _) = do f <- oftenUsually skip mutate
                                             f c
                                           where mutate  z = mutateAbiCall z >>= \c' -> pure $ t { _call = SolCall c' }
                                                 skip    _ = pure t
mutateTx t                              = pure t

-- | Lift an action in the context of a component of some 'MonadState' to an action in the
-- 'MonadState' itself.
liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = stateST . runState . zoom hasLens
  -- This is the default state function written in terms of get and set:
  where stateST f = do
          s <- get
          let ~(a, s') = f s
          put s'
          return a

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: (MonadState x m, Has VM x) => Tx -> m ()
setupTx (Tx NoCall _ r _ _ _ (t, b)) = liftSH . sequence_ $
  [ state . pc .= 0, state . stack .= mempty, state . memory .= mempty
  , block . timestamp += litWord t, block . number += b, loadContract r]

setupTx (Tx c s r g gp v (t, b)) = liftSH . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . memory .= mempty, state . gas .= g
  , tx . gasprice .= gp, tx . origin .= s, state . caller .= litAddr s, state . callvalue .= litWord v
  , block . timestamp += litWord t, block . number += b, setup] where
    setup = case c of
      SolCreate bc   -> assign (env . contracts . at r) (Just $ initialContract (InitCode (ConcreteBuffer bc)) & set balance v) >> loadContract r >> state . code .= ConcreteBuffer bc
      SolCall cd     -> incrementBalance >> loadContract r >> state . calldata .= concreteCalldata (encode cd)
      SolCalldata cd -> incrementBalance >> loadContract r >> state . calldata .= concreteCalldata cd
      NoCall         -> error "NoCall"
    incrementBalance = (env . contracts . ix r . balance) += v
    encode (n, vs) = abiCalldata
      (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs

concreteCalldata :: BS.ByteString -> (Buffer, SymWord)
concreteCalldata cd = (ConcreteBuffer cd, w256lit . fromIntegral . BS.length $ cd)
