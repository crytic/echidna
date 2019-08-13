{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (join, liftM2, liftM3, liftM5)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, runState)
import Data.Aeson (ToJSON(..), object)
import Data.ByteString (ByteString)
import Data.Either (either, lefts)
import Data.Has (Has(..))
import Data.List (intercalate)
import Data.Set (Set)
import EVM
import EVM.Concrete (Word(..), w256)
import EVM.Types (Addr)

import qualified Control.Monad.State.Strict as S (state)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Echidna.ABI
import Echidna.ABIv2

-- | A transaction is either a @CREATE@ or a regular call with an origin, destination, and value.
-- Note: I currently don't model nonces or signatures here.
data Tx2 = Tx2 { _call2  :: Either SolCall2 ByteString -- | Either a call or code for a @CREATE@
               , _src2   :: Addr                       -- | Origin
               , _dst2   :: Addr                       -- | Destination
               , _gas2'  :: Word                       -- | Gas
               , _value2 :: Word                       -- | Value
               } deriving (Eq, Ord, Show)

makeLenses ''Tx2

data TxConf = TxConf { _propGas :: Word
                     -- ^ Gas to use evaluating echidna properties
                     , _txGas   :: Word
                     -- ^ Gas to use in generated transactions
                     }

makeLenses 'TxConf

-- | Pretty-print some 'AbiCall'.
ppSolCall2 :: SolCall2 -> String
ppSolCall2 (t, vs) = (if t == "" then T.unpack "*fallback*" else T.unpack t) ++ "(" ++ intercalate "," (ppAbiValue2 <$> vs) ++ ")"

instance ToJSON Tx2 where
  toJSON (Tx2 c s d g v) = object [ ("call",  toJSON $ either ppSolCall2 (const "<CREATE>") c)
                                  -- from/to are Strings, since JSON doesn't support hexadecimal notation
                                  , ("from",  toJSON $ show s)
                                  , ("to",    toJSON $ show d)
                                  , ("value", toJSON $ show v)
                                  , ("gas",   toJSON $ show g)
                                  ]

-- | A contract is just an address with an ABI (for our purposes).
type ContractA2 = (Addr, [SolSignature2])

-- | The world is made our of humans with an address, and contracts with an address + ABI.
data World2 = World2 { _senders2   :: [Addr]
                     , _receivers2 :: [ContractA2]
                     }
  deriving (Show)
makeLenses ''World2

-- | Given generators for an origin, destination, value, and function call, generate a call
-- transaction. Note: This doesn't generate @CREATE@s because I don't know how to do that at random.
genTxWith2 :: (MonadRandom m, MonadState x m, Has World2 x, MonadThrow m) 
           => ([Addr] -> m Addr)                         -- ^ Sender generator
           -> ([ContractA2] -> m ContractA2)             -- ^ Receiver generator
           -> (Addr -> ContractA2 -> m SolCall2)         -- ^ Call generator
           -> m Word                                     -- ^ Gas generator
           -> (Addr -> ContractA2 -> SolCall2 -> m Word) -- ^ Value generator
           -> m Tx2
genTxWith2 s r c g v = use hasLens >>=
  \case (World2 ss rs) -> let s' = s ss; r' = r rs; c' = join $ liftM2 c s' r' in
                            liftM5 Tx2 (Left <$> c') s' (fst <$> r') g =<< liftM3 v s' r' c'

-- | Synthesize a random 'Transaction', not using a dictionary.
genTx2 :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World2 y, MonadThrow m) => m Tx2
genTx2 = view (hasLens . txGas) >>= \g -> genTxWith2 (rElem "sender list") (rElem "recipient list")
                                                     (const $ genInteractions2 . snd) (pure g) (\_ _ _ -> pure 0)

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTxM2 :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has GenDict2 y, Has World2 y, MonadThrow m) => m Tx2
genTxM2 = view (hasLens . txGas) >>= \g -> genTxWith2 (rElem "sender list") (rElem "recipient list")
                                                      (const $ genInteractionsM2 . snd) (pure g) (\_ _ _ -> pure 0)

-- | Check if a 'Transaction' is as \"small\" (simple) as possible (using ad-hoc heuristics).
canShrinkTx2 :: Tx2 -> Bool
canShrinkTx2 (Tx2 (Right _) _ _ _ 0)    = False
canShrinkTx2 (Tx2 (Left (_,l)) _ _ _ 0) = any canShrinkAbiValue2 l
canShrinkTx2 _                          = True

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx2 :: MonadRandom m => Tx2 -> m Tx2
shrinkTx2 (Tx2 c s d g (C _ v)) = let c' = either (fmap Left . shrinkAbiCall2) (fmap Right . pure) c in
  liftM5 Tx2 c' (pure s) (pure d) (pure g) $ w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral v)

-- | Given a 'Set' of 'Transaction's, generate a similar 'Transaction' at random.
spliceTxs2 :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World2 y, MonadThrow m) => Set Tx2 -> m Tx2
spliceTxs2 ts = let l = S.toList ts; (cs, ss) = unzip $ (\(Tx2 c s _ _ _) -> (c,s)) <$> l in
  view (hasLens . txGas) >>= \g ->
    genTxWith2 (const . rElem "sender list" $ ss) (rElem "recipient list")
               (\_ _ -> mutateAbiCall2 =<< rElem "past calls" (lefts cs)) (pure g)
               (\ _ _ (n,_) -> let valOf (Tx2 c _ _ _ v) = if elem n $ c ^? _Left . _1 then v else 0
                               in rElem "values" $ valOf <$> l)

-- | Lift an action in the context of a component of some 'MonadState' to an action in the
-- 'MonadState' itself.
liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = S.state . runState . zoom hasLens

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx2 :: (MonadState x m, Has VM x) => Tx2 -> m ()
setupTx2 (Tx2 c s r g v) = liftSH . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . memory .= mempty, state . gas .= g
  , tx . origin .= s, state . caller .= s, state . callvalue .= v, setup] where
    setup = case c of
      Left cd  -> loadContract r >> state . calldata .= encode cd
      Right bc -> assign (env . contracts . at r) (Just $ initialContract (RuntimeCode bc) & set balance v) >> loadContract r
    encode (n, vs) = abiCalldata2
      (encodeSig2 (n, abiValueType2 <$> vs)) $ V.fromList vs
