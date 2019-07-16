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
import EVM.ABI (abiCalldata, abiTypeSolidity, abiValueType)
import EVM.Concrete (Word(..), w256)
import EVM.Types (Addr)

import qualified Control.Monad.State.Strict as S (state)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Echidna.ABI

-- | A transaction is either a @CREATE@ or a regular call with an origin, destination, and value.
-- Note: I currently don't model nonces or signatures here.
data Tx = Tx { _call  :: Either SolCall ByteString -- | Either a call or code for a @CREATE@
             , _src   :: Addr                      -- | Origin
             , _dst   :: Addr                      -- | Destination
             , _gas'  :: Word                      -- | Gas
             , _value :: Word                      -- | Value
             } deriving (Eq, Ord, Show)

makeLenses ''Tx

data TxConf = TxConf { _propGas :: Word
                     -- ^ Gas to use evaluating echidna properties
                     , _txGas   :: Word
                     -- ^ Gas to use in generated transactions
                     }

makeLenses 'TxConf

-- | Pretty-print some 'AbiCall'.
ppSolCall :: SolCall -> String
ppSolCall (t, vs) = (if t == "" then T.unpack "*fallback*" else T.unpack t) ++ "(" ++ intercalate "," (ppAbiValue <$> vs) ++ ")"

instance ToJSON Tx where
  toJSON (Tx c s d g v) = object [ ("call",  toJSON $ either ppSolCall (const "<CREATE>") c)
                                 -- from/to are Strings, since JSON doesn't support hexadecimal notation
                                 , ("from",  toJSON $ show s)
                                 , ("to",    toJSON $ show d)
                                 , ("value", toJSON $ show v)
                                 , ("gas",   toJSON $ show g)
                                 ]

-- | A contract is just an address with an ABI (for our purposes).
type ContractA = (Addr, [SolSignature])

-- | The world is made our of humans with an address, and contracts with an address + ABI.
data World = World { _senders   :: [Addr]
                   , _receivers :: [ContractA]
                   }

makeLenses ''World

-- | Given generators for an origin, destination, value, and function call, generate a call
-- transaction. Note: This doesn't generate @CREATE@s because I don't know how to do that at random.
genTxWith :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m) 
          => ([Addr] -> m Addr)                       -- ^ Sender generator
          -> ([ContractA] -> m ContractA)             -- ^ Receiver generator
          -> (Addr -> ContractA -> m SolCall)         -- ^ Call generator
          -> m Word                                   -- ^ Gas generator
          -> (Addr -> ContractA -> SolCall -> m Word) -- ^ Value generator
          -> m Tx
genTxWith s r c g v = use hasLens >>=
  \case (World ss rs) -> let s' = s ss; r' = r rs; c' = join $ liftM2 c s' r' in
                           liftM5 Tx (Left <$> c') s' (fst <$> r') g =<< liftM3 v s' r' c'

-- | Synthesize a random 'Transaction', not using a dictionary.
genTx :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World y, MonadThrow m) => m Tx
genTx = view (hasLens . txGas) >>= \g -> genTxWith (rElem "sender list") (rElem "recipient list")
                                                   (const $ genInteractions . snd) (pure g) (\_ _ _ -> pure 0)

-- | Generate a random 'Transaction' with either synthesis or mutation of dictionary entries.
genTxM :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has GenDict y, Has World y, MonadThrow m) => m Tx
genTxM = view (hasLens . txGas) >>= \g -> genTxWith (rElem "sender list") (rElem "recipient list")
                                                    (const $ genInteractionsM . snd) (pure g) (\_ _ _ -> pure 0)

-- | Check if a 'Transaction' is as \"small\" (simple) as possible (using ad-hoc heuristics).
canShrinkTx :: Tx -> Bool
canShrinkTx (Tx (Right _) _ _ _ 0)    = False
canShrinkTx (Tx (Left (_,l)) _ _ _ 0) = any canShrinkAbiValue l
canShrinkTx _                         = True

-- | Given a 'Transaction', generate a random \"smaller\" 'Transaction', preserving origin,
-- destination, value, and call signature.
shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx (Tx c s d g (C _ v)) = let c' = either (fmap Left . shrinkAbiCall) (fmap Right . pure) c in
  liftM5 Tx c' (pure s) (pure d) (pure g) $ w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral v)

-- | Given a 'Set' of 'Transaction's, generate a similar 'Transaction' at random.
spliceTxs :: (MonadRandom m, MonadReader x m, Has TxConf x, MonadState y m, Has World y, MonadThrow m) => Set Tx -> m Tx
spliceTxs ts = let l = S.toList ts; (cs, ss) = unzip $ (\(Tx c s _ _ _) -> (c,s)) <$> l in
  view (hasLens . txGas) >>= \g ->
    genTxWith (const . rElem "sender list" $ ss) (rElem "recipient list")
              (\_ _ -> mutateAbiCall =<< rElem "past calls" (lefts cs)) (pure g)
              (\ _ _ (n,_) -> let valOf (Tx c _ _ _ v) = if elem n $ c ^? _Left . _1 then v else 0
                              in rElem "values" $ valOf <$> l)

-- | Lift an action in the context of a component of some 'MonadState' to an action in the
-- 'MonadState' itself.
liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = S.state . runState . zoom hasLens

-- | Given a 'Transaction', set up some 'VM' so it can be executed. Effectively, this just brings
-- 'Transaction's \"on-chain\".
setupTx :: (MonadState x m, Has VM x) => Tx -> m ()
setupTx (Tx c s r g v) = liftSH . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . memory .= mempty, state . gas .= g
  , tx . origin .= s, state . caller .= s, state . callvalue .= v, setup] where
    setup = case c of
      Left cd  -> loadContract r >> state . calldata .= encode cd
      Right bc -> assign (env . contracts . at r) (Just $ initialContract (RuntimeCode bc) & set balance v) >> loadContract r
    encode (n, vs) = abiCalldata
      (n <> "(" <> T.intercalate "," (abiTypeSolidity . abiValueType <$> vs) <> ")") $ V.fromList vs
