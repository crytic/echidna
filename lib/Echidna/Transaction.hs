{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (liftM4)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, runState)
import Data.ByteString (ByteString)
import Data.Either (either, lefts)
import Data.Has (Has(..))
import Data.Set (Set)
import EVM
import EVM.ABI (abiCalldata, abiTypeSolidity, abiValueType)
import EVM.Concrete (Blob(..), Word(..), w256)
import EVM.Types (Addr)

import qualified Control.Monad.State.Strict as S (state)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

import Echidna.ABI

-- Note: I currently don't model gas cost, nonces, or signatures here
data Tx = Tx { _call  :: Either SolCall ByteString
             , _src   :: Addr
             , _dst   :: Addr
             , _value :: Word
             } deriving (Eq, Ord, Show)

makeLenses ''Tx

type ContractA = (Addr, [SolSignature])

data World = World { _senders   :: [Addr]
                   , _receivers :: [ContractA]
                   }

makeLenses ''World

genTxWith :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m) 
          => ([Addr] -> m Addr)                       -- Sender generator
          -> ([ContractA] -> m ContractA)             -- Receiver generator
          -> (Addr -> ContractA -> m SolCall)         -- Call generator
          -> (Addr -> ContractA -> SolCall -> m Word) -- Value generator
          -> m Tx
genTxWith s r c v = use hasLens >>= \case(World ss rs) -> do s' <- s ss
                                                             r' <- r rs
                                                             c' <- c s' r'
                                                             Tx (Left c') s' (fst r') <$> v s' r' c'

genTx :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m) => m Tx
genTx = genTxWith (rElem "sender list") (rElem "recipient list") (const $ genInteractions . snd) (\_ _ _ -> pure 0)

genTxM :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m, MonadReader y m, Has GenConf y) => m Tx
genTxM = genTxWith (rElem "sender list") (rElem "recipient list") (const $ genInteractionsM . snd) (\_ _ _ -> pure 0)

canShrinkTx :: Tx -> Bool
canShrinkTx (Tx (Right _) _ _ 0)    = False
canShrinkTx (Tx (Left (_,l)) _ _ 0) = any canShrinkAbiValue l
canShrinkTx _                       = True

shrinkTx :: MonadRandom m => Tx -> m Tx
shrinkTx (Tx c s d (C _ v)) = let c' = either (fmap Left . shrinkAbiCall) (fmap Right . pure) c in
  liftM4 Tx c' (pure s) (pure d) $ w256 . fromIntegral <$> getRandomR (0 :: Integer, fromIntegral v)

spliceTxs :: (MonadRandom m, MonadState x m, Has World x, MonadThrow m) => Set Tx -> m Tx
spliceTxs ts = let l = S.toList ts; (cs, ss) = unzip $ (\(Tx c s _ _) -> (c,s)) <$> l in
  genTxWith (const . rElem "sender list" $ ss) (rElem "recipient list")
            (\_ _ -> mutateAbiCall =<< rElem "past calls" (lefts cs))
            (\ _ _ (n,_) -> let valOf (Tx c _ _ v) = if elem n $ c ^? _Left . _1 then v else 0
                            in rElem "values" $ valOf <$> l)

liftSH :: (MonadState a m, Has b a) => State b x -> m x
liftSH = S.state . runState . zoom hasLens

setupTx :: (MonadState x m, Has VM x) => Tx -> m ()
setupTx (Tx c s r v) = liftSH . sequence_ $
  [ result .= Nothing, state . pc .= 0, state . stack .= mempty, state . gas .= 0xffffffff
  , env . origin .= s, state . caller .= s, state . callvalue .= v, setup] where
    setup = case c of
      Left cd  -> loadContract r >> state . calldata .= encode cd
      Right bc -> assign (env . contracts . ix r) (initialContract bc) >> loadContract r
    encode (n, vs) = B . abiCalldata
      (n <> "(" <> T.intercalate "," (abiTypeSolidity . abiValueType <$> vs) <> ")") $ V.fromList vs
