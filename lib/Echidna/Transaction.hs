{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.Transaction where

import Prelude hiding (Word)

import Control.Lens
import Control.Monad (join, liftM2, liftM3, liftM5, forM_, replicateM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Random.Strict (MonadRandom, getRandomR)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Strict (MonadState, State, runState)
import Data.Aeson (ToJSON(..), object)
import Data.Binary.Put (Put, runPut, putWord8, putWord32be)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.Either (either, lefts)
import Data.Has (Has(..))
import Data.List (intercalate)
import Data.Set (Set)
import Data.Text.Encoding (encodeUtf8)
import EVM
import EVM.Concrete (Word(..), w256)
import EVM.Keccak (abiKeccak)
import EVM.Types (Addr)

import qualified Control.Monad.State.Strict as S (state)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
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

abiCalldata2 :: T.Text -> V.Vector AbiValue2 -> BS.ByteString
abiCalldata2 s xs = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s))
  putAbiSeq2 xs

putAbi2 :: AbiValue2 -> Put
putAbi2 = \case
  AbiUInt2 n x -> do
    let word32Count = div (roundTo256Bits n) 4
    forM_ (reverse [0 .. word32Count - 1]) $ \i ->
      putWord32be (fromIntegral (shiftR x (i * 32) .&. 0xffffffff))

  AbiInt2 n x   -> putAbi2 (AbiUInt2 n (fromIntegral x))
  AbiAddress2 x -> putAbi2 (AbiUInt2 160 (fromIntegral x))
  AbiBool2 x    -> putAbi2 (AbiUInt2 8 (if x then 1 else 0))

  AbiBytes2 n xs -> do
    forM_ [0 .. n-1] (putWord8 . BS.index xs)
    replicateM_ (roundTo256Bits n - n) (putWord8 0)

  AbiBytesDynamic2 xs -> do
    let n = BS.length xs
    putAbi2 (AbiUInt2 256 (fromIntegral n))
    putAbi2 (AbiBytes2 n xs)

  AbiString2 s ->
    putAbi2 (AbiBytesDynamic2 s)

  AbiArray2 _ _ xs ->
    putAbiSeq2 xs

  AbiArrayDynamic2 _ xs -> do
    putAbi2 (AbiUInt2 256 (fromIntegral (V.length xs)))
    putAbiSeq2 xs

  AbiTuple2 v ->
    putAbiSeq2 v

putAbiSeq2 :: V.Vector AbiValue2 -> Put
putAbiSeq2 xs =
  do snd $ V.foldl' f (headSize, pure ()) (V.zip xs tailSizes)
     V.sequence_ (V.map putAbiTail2 xs)
  where
    headSize = V.sum $ V.map abiHeadSize2 xs
    tailSizes = V.map abiTailSize2 xs
    -- f is like a putHead
    f (i, m) (x, j) =
      case abiKind2 (abiValueType2 x) of
        Static -> (i, m >> putAbi2 x)
        Dynamic -> (i + j, m >> putAbi2 (AbiUInt2 256 (fromIntegral i)))

putAbiTail2 :: AbiValue2 -> Put
putAbiTail2 x =
  case abiKind2 (abiValueType2 x) of
    -- static types always have tail = ""
    Static  -> pure ()
    -- dynamic types (even in the case of tuple) just get encoded and inserted
    Dynamic -> putAbi2 x

abiHeadSize2 :: AbiValue2 -> Int
abiHeadSize2 x =
  case abiKind2 (abiValueType2 x) of
    -- even for dynamic tuples it's just a len() invocation, which is uint256
    Dynamic -> 32
      --case x of
      --     AbiTuple2 v -> 32 * length v
      --     _           -> 32
    Static ->
      case x of
        AbiUInt2 n _  -> roundTo256Bits n
        AbiInt2  n _  -> roundTo256Bits n
        AbiBytes2 n _ -> roundTo256Bits n
        AbiAddress2 _ -> 32
        AbiBool2 _    -> 32
        AbiArray2 _ _ xs -> V.sum (V.map abiHeadSize2 xs) +
                            V.sum (V.map abiTailSize2 xs)
        AbiBytesDynamic2 _ -> 32
        AbiArrayDynamic2 _ _ -> 32
        AbiString2 _       -> 32
        AbiTuple2 v   -> sum (abiHeadSize2 <$> v) +
                         sum (abiTailSize2 <$> v) -- pretty sure this is just 0 but w/e

abiTailSize2 :: AbiValue2 -> Int
abiTailSize2 x =
  case abiKind2 (abiValueType2 x) of
    Static -> 0
    Dynamic ->
      case x of
        AbiString2 s -> 32 + roundTo256Bits (BS.length s)
        AbiBytesDynamic2 s -> 32 + roundTo256Bits (BS.length s)
        AbiArrayDynamic2 _ xs -> 32 + V.sum (V.map abiValueSize2 xs)
        AbiArray2 _ _ xs -> V.sum (V.map abiValueSize2 xs)
        AbiTuple2 v -> sum (abiValueSize2 <$> v)
        _ -> error "impossible"

abiValueSize2 :: AbiValue2 -> Int
abiValueSize2 x =
  case x of
    AbiUInt2 n _  -> roundTo256Bits n
    AbiInt2  n _  -> roundTo256Bits n
    AbiBytes2 n _ -> roundTo256Bits n
    AbiAddress2 _ -> 32
    AbiBool2 _    -> 32
    AbiArray2 _ _ xs -> V.sum (V.map abiHeadSize2 xs) +
                        V.sum (V.map abiTailSize2 xs)
    AbiBytesDynamic2 xs -> 32 + roundTo256Bits (BS.length xs)
    AbiArrayDynamic2 _ xs -> 32 + V.sum (V.map abiHeadSize2 xs) +
                                  V.sum (V.map abiTailSize2 xs)
    AbiString2 s -> 32 + roundTo256Bits (BS.length s)
    AbiTuple2 v  -> sum (abiValueSize2 <$> v)

roundTo256Bits :: Integral a => a -> a
roundTo256Bits n = 32 * div (n + 255) 256
