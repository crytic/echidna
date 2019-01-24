{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Echidna.ABI where

import Control.Applicative ((<**>))
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.Random.Strict
import Data.Bits (Bits(..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Has (Has(..))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.List (group, sort)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word8 (Word8)
import EVM.ABI (AbiType(..), AbiValue(..), abiValueType)

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V

-- Quick helper

newtype ElemException = ElemException String

instance Show ElemException where
  show (ElemException s) = "Exception: tried to get element of " ++ s ++ ", but it was empty!"

instance Exception ElemException

rElem :: (MonadThrow m, MonadRandom m) => String -> [a] -> m a
rElem s [] = throwM $ ElemException s
rElem _ l  = (l !!) <$> getRandomR (0, length l - 1)

-- Types

-- Don't construct this directly! Use mkConf

type SolCall      = (Text, [AbiValue])
type SolSignature = (Text, [AbiType])

data GenConf = GenConf { pSynthA    :: Float                          -- Fraction of time to use dictionary vs. synthesize
                       , constants  :: HashMap AbiType [AbiValue]     -- Constants to use, sorted by type
                       , wholeCalls :: HashMap SolSignature [SolCall] -- Whole calls to use, sorted by type
                       }

-- This instance is the only way for mkConf to work nicely, and is well-formed.
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}
-- We need the above since hlint doesn't notice DeriveAnyClass in StandaloneDeriving.
deriving instance Hashable AbiType

mkConf :: Float -> [AbiValue] -> [SolCall] -> GenConf
mkConf p vs cs = GenConf p (tsOf id vs) (tsOf (fmap . fmap) cs) where
  tsOf f = M.fromList . mapMaybe (liftM2 fmap (\l x -> (f abiValueType x, l)) listToMaybe) . group . sort

-- Generation (synthesis)

genAbiValue :: MonadRandom m => AbiType -> m AbiValue
genAbiValue (AbiUIntType n) = AbiUInt n  . fromInteger <$> getRandomR (0, 2 ^ n - 1)
genAbiValue (AbiIntType n)  = AbiInt n   . fromInteger <$> getRandomR (-1 * 2 ^ n, 2 ^ (n - 1))
genAbiValue AbiAddressType  = AbiAddress . fromInteger <$> getRandomR (0, 2 ^ (160 :: Integer) - 1)
genAbiValue AbiBoolType     = AbiBool <$> getRandom
genAbiValue (AbiBytesType n)    = AbiBytes n . BS.pack . take n <$> getRandoms
genAbiValue AbiBytesDynamicType = liftM2 (\n -> AbiBytesDynamic . BS.pack . take n) (getRandomR (1, 32)) getRandoms
genAbiValue AbiStringType       = liftM2 (\n -> AbiString       . BS.pack . take n) (getRandomR (1, 32)) getRandoms
genAbiValue (AbiArrayType n t)      = AbiArray n t <$> V.replicateM n (genAbiValue t)
genAbiValue (AbiArrayDynamicType t) = fmap (AbiArrayDynamic t) $ getRandomR (1, 32) >>= flip V.replicateM (genAbiValue t)

genAbiCall :: MonadRandom m => SolSignature -> m SolCall
genAbiCall = traverse $ mapM genAbiValue

genInteractions :: (MonadThrow m, MonadRandom m) => [SolSignature] -> m SolCall
genInteractions l = genAbiCall =<< rElem "ABI" l

-- Mutation helper functions

mutateNum :: (Integral a, Num a, MonadRandom m) => a -> m a
mutateNum x = bool (+) (-) <$> getRandom <*> pure x <*> (fromIntegral <$> getRandomR (0 :: Integer, fromIntegral x))

addChars :: MonadRandom m => m Word8 -> ByteString -> m ByteString
addChars c b = foldM withR b . enumFromTo 0 =<< rand where
  rand       = getRandomR (0, BS.length b)
  withR b' n = (\x -> BS.take n b' <> BS.singleton x <> BS.drop (n + 1) b') <$> c

addNulls :: MonadRandom m => ByteString -> m ByteString
addNulls = addChars $ pure 0

shrinkWith :: MonadRandom m => (Int -> a -> a) -> (Int -> a -> a) -> (a -> Int) -> a -> m a
shrinkWith f g l t = let rand = getRandomR (0, l t) in liftM2 (\x y -> f x $ g y t) rand rand

shrinkBS :: MonadRandom m => ByteString -> m ByteString
shrinkBS = shrinkWith BS.take BS.drop BS.length

shrinkL :: MonadRandom m => [a] -> m [a]
shrinkL = shrinkWith take drop length

shrinkV :: MonadRandom m => Vector a -> m (Vector a)
shrinkV = shrinkWith V.take V.drop V.length

growWith :: MonadRandom m => m b -> (b -> a -> a) -> (a -> b -> a) -> (a -> Int) -> a -> m a
growWith m f g l t = foldM withR t =<< flip replicateM m =<< rand where
  rand       = getRandomR (0, l t)
  withR t' x = bool (f x t') (g t' x) <$> getRandom

mutateBS :: MonadRandom m => ByteString -> m ByteString
mutateBS b = addChars getRandom =<< changeSize where
  changeSize = bool (shrinkBS b) (growWith getRandom BS.cons BS.snoc BS.length b) =<< getRandom

mutateV :: MonadRandom m => AbiType -> Vector AbiValue -> m (Vector AbiValue)
mutateV t v = mapM mutateAbiValue =<< changeSize where
  changeSize = bool (shrinkV v) (growWith (genAbiValue t) V.cons V.snoc V.length v) =<< getRandom

-- Mutation

canShrinkAbiValue :: AbiValue -> Bool
canShrinkAbiValue (AbiUInt _ 0) = False
canShrinkAbiValue (AbiInt  _ 0) = False
canShrinkAbiValue (AbiBool b) = b
canShrinkAbiValue (AbiBytes _ b)      = BS.any (/= 0) b
canShrinkAbiValue (AbiBytesDynamic "") = True
canShrinkAbiValue (AbiString "")       = True
canShrinkAbiValue (AbiArray _ _ l)      = any canShrinkAbiValue l
canShrinkAbiValue (AbiArrayDynamic _ l) = l == mempty
canShrinkAbiValue _ = True

dropBits :: forall a m. (Bits a, Bounded a, Integral a, MonadRandom m) => a -> m a
dropBits x = (x .&.) . fromIntegral <$> getRandomR bounds where
  bounds :: (Integer, Integer)
  bounds = (fromIntegral (minBound :: a), fromIntegral (maxBound :: a))

shrinkAbiValue :: MonadRandom m => AbiValue -> m AbiValue
shrinkAbiValue (AbiUInt n m) = AbiUInt n <$> dropBits m
shrinkAbiValue (AbiInt n m)  = AbiInt n  <$> dropBits m
shrinkAbiValue x@AbiAddress{} = pure x
shrinkAbiValue (AbiBool _)    = pure $ AbiBool False
shrinkAbiValue (AbiBytes n b)      = AbiBytes n <$> addNulls b
shrinkAbiValue (AbiBytesDynamic b) = fmap AbiBytesDynamic $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiString b)       = fmap AbiString       $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiArray n t l)      = AbiArray n t <$> mapM shrinkAbiValue l
shrinkAbiValue (AbiArrayDynamic t l) = fmap (AbiArrayDynamic t) $ mapM shrinkAbiValue =<< shrinkV l

shrinkAbiCall :: MonadRandom m => SolCall -> m SolCall
shrinkAbiCall = traverse $ mapM shrinkAbiValue

mutateAbiValue :: MonadRandom m => AbiValue -> m AbiValue
mutateAbiValue (AbiUInt n x)  = AbiUInt n <$> mutateNum x
mutateAbiValue (AbiInt n x)   = AbiInt n  <$> mutateNum x
mutateAbiValue (AbiAddress _) = genAbiValue AbiAddressType
mutateAbiValue (AbiBool _)    = genAbiValue AbiBoolType
mutateAbiValue (AbiBytes n b)        = AbiBytes n        <$> addChars getRandom b
mutateAbiValue (AbiBytesDynamic b)   = AbiBytesDynamic   <$> mutateBS b
mutateAbiValue (AbiString b)         = AbiString         <$> mutateBS b
mutateAbiValue (AbiArray n t l)      = AbiArray n t      <$> mapM mutateAbiValue l
mutateAbiValue (AbiArrayDynamic t l) = AbiArrayDynamic t <$> mutateV t l

mutateAbiCall :: MonadRandom m => SolCall -> m SolCall
mutateAbiCall = traverse $ mapM mutateAbiValue

-- Generation, with dictionary

genWithDict :: (Eq a, Hashable a, MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m)
            => (GenConf -> HashMap a [b]) -> (a -> m b) -> a -> m b
genWithDict f g t = asks getter >>= \c -> do
  useD <- (pSynthA c <) <$> getRandom
  g t <**> case (M.lookup t (f c), useD) of (Just l@(_:_), True) -> const <$> rElem "" l
                                            _                    -> pure id

genAbiValueM :: (MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m) => AbiType -> m AbiValue
genAbiValueM = genWithDict constants genAbiValue

genAbiCallM :: (MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m) => SolSignature -> m SolCall
genAbiCallM = genWithDict wholeCalls genAbiCall

genInteractionsM :: (MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m) => [SolSignature] -> m SolCall
genInteractionsM l = genAbiCallM =<< rElem "ABI" l
