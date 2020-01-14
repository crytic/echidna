{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (join, liftM2, liftM3, liftM5)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, evalStateT, runState)
import Data.Either (either)
import Data.Has (Has(..))
import EVM hiding (value)
import EVM.ABI (abiCalldata, abiValueType)
import EVM.Concrete (Word(..), w256)
import EVM.Types (Addr)

import qualified Control.Monad.State.Strict as S (state)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import Echidna.ABI
import Echidna.Defaults (defaultDict)
import Echidna.Solidity.Types (SolSignature, SolCall)
import Echidna.Types (GenDict, Tx(Tx), call, value, gasprice', delay)
import Echidna.Util (encodeSig)

data TxConf = TxConf { _propGas       :: Word
                     -- ^ Gas to use evaluating echidna properties
                     , _txGas         :: Word
                     -- ^ Gas to use in generated transactions
                     , _maxGasprice   :: Word
                     -- ^ Maximum gasprice to be checked for a transaction
                     , _maxTimeDelay  :: Word
                     -- ^ Maximum time delay between transactions (seconds)
                     , _maxBlockDelay :: Word
                     -- ^ Maximum block delay between transactions
                     }

makeLenses 'TxConf

-- | If half a tuple is zero, make both halves zero. Useful for generating delays, since block number
-- only goes up with timestamp
level :: (Num a, Eq a) => (a, a) -> (a, a)
level (elemOf each 0 -> True) = (0,0)
level x                       = x

-- | A contract is just an address with an ABI (for our purposes).
type ContractA = (Addr, NE.NonEmpty SolSignature)

-- | The world is made our of humans with an address, and contracts with an address + ABI.
data World = World { _senders   :: NE.NonEmpty Addr
                   , _receivers :: NE.NonEmpty ContractA
                   }
makeLenses ''World

-- | Given generators for an origin, destination, value, and function call, generate a call
-- transaction. Note: This doesn't generate @CREATE@s because I don't know how to do that at random.
genTxWith :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m)
          => (NE.NonEmpty Addr -> m Addr)             -- ^ Sender generator
          -> (NE.NonEmpty ContractA -> m ContractA)   -- ^ Receiver generator
          -> (Addr -> ContractA -> m SolCall)         -- ^ Call generator
          -> m Word                                   -- ^ Gas generator
          -> m Word                                   -- ^ Gas price generator
          -> (Addr -> ContractA -> SolCall -> m Word) -- ^ Value generator
          -> m (Word, Word)                           -- ^ Delay generator
          -> m Tx
genTxWith s r c g gp v t = use hasLens >>= \(World ss rs) ->
  let s' = s ss; r' = r rs; c' = join $ liftM2 c s' r' in
    ((liftM5 Tx (Left <$> c') s' (fst <$> r') g gp <*>) =<< liftM3 v s' r' c') <*> t

-- | Synthesize a random 'Transaction', not using a dictionary.
genTx :: forall m x y. (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World y, MonadThrow m) => m Tx
genTx = use (hasLens :: Lens' y World) >>= evalStateT genTxM . (defaultDict,)

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTxM :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has GenDict y, Has World y, MonadThrow m) => m Tx
genTxM = view hasLens >>= \(TxConf _ g maxGp t b) -> genTxWith
  rElem rElem                                                                -- src and dst
  (const $ genInteractionsM . snd)                                           -- call itself
  (pure g) (inRange maxGp) (\_ _ _ -> pure 0)                                -- gas, gasprice, value
  (level <$> liftM2 (,) (inRange t) (inRange b))                             -- delay
     where inRange hi = w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral hi)

-- | Check if a 'Transaction' is as \"small\" (simple) as possible (using ad-hoc heuristics).
canShrinkTx :: Tx -> Bool
canShrinkTx (Tx (Right _) _ _ _ 0 0 (0, 0))    = False
canShrinkTx (Tx (Left (_,l)) _ _ _ 0 0 (0, 0)) = any canShrinkAbiValue l
canShrinkTx _                                = True

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx tx'@(Tx c _ _ _ gp (C _ v) (C _ t, C _ b)) = let
  c' = either (fmap Left . shrinkAbiCall) (fmap Right . pure) c
  lower x = w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral x)
  possibilities =
    [ set call      <$> c'
    , set value     <$> lower v
    , set gasprice' <$> lower gp
    , set delay     <$> fmap level (liftM2 (,) (lower t) (lower b))
    ]
  in join (uniform possibilities) <*> pure tx'

-- | Lift an action in the context of a component of some 'MonadState' to an action in the
-- 'MonadState' itself.
liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = S.state . runState . zoom hasLens

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: (MonadState x m, Has VM x) => Tx -> m ()
setupTx (Tx c s r g gp v (t, b)) = liftSH . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . memory .= mempty, state . gas .= g
  , tx . gasprice .= gp, tx . origin .= s, state . caller .= s, state . callvalue .= v
  , block . timestamp += t, block . number += b, setup] where
    setup = case c of
      Left cd  -> loadContract r >> state . calldata .= encode cd
      Right bc -> assign (env . contracts . at r) (Just $ initialContract (RuntimeCode bc) & set balance v) >> loadContract r
    encode (n, vs) = abiCalldata
      (encodeSig (n, abiValueType <$> vs)) $ V.fromList vs
