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

-- Safe random element of a list

-- | Thrown when trying to pick a random element of an empty list. The 'String' describes the list.
newtype ElemException = ElemException String

instance Show ElemException where
  show (ElemException s) = "Exception: tried to get element of " ++ s ++ ", but it was empty!"

instance Exception ElemException

-- | Get a random element of a list, or throw 'ElemException' if the list is empty.
rElem :: (MonadThrow m, MonadRandom m) => String -> [a] -> m a
rElem s [] = throwM $ ElemException s
rElem _ l  = (l !!) <$> getRandomR (0, length l - 1)

-- Types

-- Don't construct this directly! Use mkConf.

-- | Represents a call to a Solidity function.
-- A tuple of 'Text' for the name of the function, and then any 'AbiValue' arguments passed (as a list).
type SolCall      = (Text, [AbiValue])

-- | Represents the type of a Solidity function.
-- A tuple of 'Text' for the name of the function, and then the 'AbiType's of any arguments it expects.
type SolSignature = (Text, [AbiType])

-- | Configuration necessary for generating new 'SolCalls'. Don't construct this by hand! Use 'mkConf'.
data GenConf = GenConf { pSynthA    :: Float
                         -- ^ Fraction of time to use dictionary vs. synthesize
                       , constants  :: HashMap AbiType [AbiValue]
                         -- ^ Constants to use, sorted by type
                       , wholeCalls :: HashMap SolSignature [SolCall] 
                         -- ^ Whole calls to use, sorted by type
                       }

-- This instance is the only way for mkConf to work nicely, and is well-formed.
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}
-- We need the above since hlint doesn't notice DeriveAnyClass in StandaloneDeriving.
deriving instance Hashable AbiType

-- | Construct a 'GenConf' from some dictionaries and a 'Float'.
mkConf :: Float      -- ^ Percentage of time to mutate instead of synthesiz. Should be in [0,1]
       -> [AbiValue] -- ^ A list of 'AbiValue' constants to use during dictionary-based generation
       -> [SolCall]  -- ^ A list of complete 'SolCall's to mutate
       -> GenConf
mkConf p vs cs = GenConf p (tsOf id vs) (tsOf (fmap . fmap) cs) where
  tsOf f = M.fromList . mapMaybe (liftM2 fmap (\l x -> (f abiValueType x, l)) listToMaybe) . group . sort

-- Generation (synthesis)

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Doesn't use a dictionary.
genAbiValue :: MonadRandom m => AbiType -> m AbiValue
genAbiValue (AbiUIntType n) = AbiUInt n  . fromInteger <$> getRandomR (0, 2 ^ n - 1)
genAbiValue (AbiIntType n)  = AbiInt n   . fromInteger <$> getRandomR (-1 * 2 ^ n, 2 ^ (n - 1))
genAbiValue AbiAddressType  = AbiAddress . fromInteger <$> getRandomR (0, 2 ^ (160 :: Integer) - 1)
genAbiValue AbiBoolType     = AbiBool <$> getRandom
genAbiValue (AbiBytesType n)    = AbiBytes n . BS.pack . take n <$> getRandoms
genAbiValue AbiBytesDynamicType = liftM2 (\n -> AbiBytesDynamic . BS.pack . take n)
                                         (getRandomR (1, 32)) getRandoms
genAbiValue AbiStringType       = liftM2 (\n -> AbiString       . BS.pack . take n)
                                         (getRandomR (1, 32)) getRandoms
genAbiValue (AbiArrayType n t)      = AbiArray n t <$> V.replicateM n (genAbiValue t)
genAbiValue (AbiArrayDynamicType t) = fmap (AbiArrayDynamic t) $ getRandomR (1, 32)
                                      >>= flip V.replicateM (genAbiValue t)

-- | Synthesize a random 'SolCall' given its 'SolSignature'. Doesn't use a dictionary.
genAbiCall :: MonadRandom m => SolSignature -> m SolCall
genAbiCall = traverse $ traverse genAbiValue

-- | Synthesize a random 'SolCall' given a list of 'SolSignature's (effectively, an ABI). Doesn't use a dictionary.
genInteractions :: (MonadThrow m, MonadRandom m) => [SolSignature] -> m SolCall
genInteractions l = genAbiCall =<< rElem "ABI" l

-- Mutation helper functions

-- | Given an 'Integral' number n, get a random number in [0,2n].
mutateNum :: (Integral a, MonadRandom m) => a -> m a
mutateNum x = bool (x +) (x -) <$> getRandom <*> (fromIntegral <$> getRandomR (0, toInteger x))

-- | Given a way to generate random 'Word8's and a 'ByteString' b of length l,
-- generate between 0 and 2l 'Word8's and add insert them into b at random indices.
addChars :: MonadRandom m => m Word8 -> ByteString -> m ByteString
addChars c b = foldM withR b . enumFromTo 0 =<< rand where
  rand       = getRandomR (0, BS.length b)
  withR b' n = (\x -> BS.take n b' <> BS.singleton x <> BS.drop (n + 1) b') <$> c

-- | Like 'addChars', but instead of adding random chars, simply adding null bytes.
addNulls :: MonadRandom m => ByteString -> m ByteString
addNulls = addChars $ pure 0

-- | Given a \"list-y\" structure with analogues of 'take', 'drop', and 'length', remove some elements at random.
shrinkWith :: MonadRandom m
           => (Int -> a -> a) -- ^ 'take' function
           -> (Int -> a -> a) -- ^ 'drop' function
           -> (a -> Int)      -- ^ 'length' function
           -> a               -- ^ Structure to shrink
           -> m a
shrinkWith f g l t = let rand = getRandomR (0, l t) in liftM2 (\x y -> f x $ g y t) rand rand

-- | Given a 'ByteString', remove some characters at random.
shrinkBS :: MonadRandom m => ByteString -> m ByteString
shrinkBS = shrinkWith BS.take BS.drop BS.length

-- | Given a 'List', remove some elements at random.
shrinkL :: MonadRandom m => [a] -> m [a]
shrinkL = shrinkWith take drop length

-- | Given a 'Vector', remove some elements at random.
shrinkV :: MonadRandom m => Vector a -> m (Vector a)
shrinkV = shrinkWith V.take V.drop V.length

-- | Given a \"list-y\" structure with analogues of 'cons', 'snoc', and 'length',
-- plus a generator for new elements, add some elements at random.
growWith :: MonadRandom m
         => m b           -- ^ Generator for elements of the structure
         -> (b -> a -> a) -- ^ 'cons' function
         -> (a -> b -> a) -- ^ 'snoc' function
         -> (a -> Int)    -- ^ 'length' function
         -> a             -- ^ Structure to grow
         -> m a
growWith m f g l t = foldM withR t =<< flip replicateM m =<< rand where
  rand       = getRandomR (0, l t)
  withR t' x = bool (f x t') (g t' x) <$> getRandom

-- | Given a 'ByteString', add and drop some characters at random.
mutateBS :: MonadRandom m => ByteString -> m ByteString
mutateBS b = addChars getRandom =<< changeSize where
  changeSize = bool (shrinkBS b) (growWith getRandom BS.cons BS.snoc BS.length b) =<< getRandom

-- | Given a 'Vector', add and drop some characters at random.
mutateV :: MonadRandom m => AbiType -> Vector AbiValue -> m (Vector AbiValue)
mutateV t v = traverse mutateAbiValue =<< changeSize where
  changeSize = bool (shrinkV v) (growWith (genAbiValue t) V.cons V.snoc V.length v) =<< getRandom

-- Mutation

-- | Check if an 'AbiValue' is as \"small\" (trivial) as possible (using ad-hoc heuristics).
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

bounds :: forall a. (Bounded a, Integral a) => a -> (Integer, Integer)
bounds = const (fromIntegral (0 :: a), fromIntegral (maxBound :: a))

-- | Given a number with a bitvector representation, randomly set some bits to zero.
dropBits :: forall a m. (Bits a, Bounded a, Integral a, MonadRandom m) => a -> m a
dropBits x = (x .&.) . fromIntegral <$> getRandomR (bounds x)

-- | Given a number with a bitvector representation, randomly set some bits to one.
incBits :: forall a m. (Bits a, Bounded a, Integral a, MonadRandom m) => a -> m a
incBits x = (x .|.) . fromIntegral <$> getRandomR (bounds x)

-- | Given an 'AbiValue', generate a random \"smaller\" (simpler) value of the same 'AbiType'.
shrinkAbiValue :: MonadRandom m => AbiValue -> m AbiValue
shrinkAbiValue (AbiUInt n m) = AbiUInt n <$> dropBits m
shrinkAbiValue (AbiInt n m)  = AbiInt n  <$> (case m `compare` 0 of {GT -> dropBits; EQ -> pure; LT -> incBits}) m
shrinkAbiValue x@AbiAddress{} = pure x
shrinkAbiValue (AbiBool _)    = pure $ AbiBool False
shrinkAbiValue (AbiBytes n b)      = AbiBytes n <$> addNulls b
shrinkAbiValue (AbiBytesDynamic b) = fmap AbiBytesDynamic $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiString b)       = fmap AbiString       $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiArray n t l)      = AbiArray n t <$> traverse shrinkAbiValue l
shrinkAbiValue (AbiArrayDynamic t l) = fmap (AbiArrayDynamic t) $ traverse shrinkAbiValue =<< shrinkV l

-- | Given a 'SolCall', generate a random \"smaller\" (simpler) call.
shrinkAbiCall :: MonadRandom m => SolCall -> m SolCall
shrinkAbiCall = traverse $ traverse shrinkAbiValue

-- | Given an 'AbiValue', generate a random \"similar\" value of the same 'AbiType'.
mutateAbiValue :: MonadRandom m => AbiValue -> m AbiValue
mutateAbiValue (AbiUInt n x)  = AbiUInt n <$> mutateNum x
mutateAbiValue (AbiInt n x)   = AbiInt n  <$> mutateNum x
mutateAbiValue (AbiAddress _) = genAbiValue AbiAddressType
mutateAbiValue (AbiBool _)    = genAbiValue AbiBoolType
mutateAbiValue (AbiBytes n b)        = AbiBytes n        <$> addChars getRandom b
mutateAbiValue (AbiBytesDynamic b)   = AbiBytesDynamic   <$> mutateBS b
mutateAbiValue (AbiString b)         = AbiString         <$> mutateBS b
mutateAbiValue (AbiArray n t l)      = AbiArray n t      <$> traverse mutateAbiValue l
mutateAbiValue (AbiArrayDynamic t l) = AbiArrayDynamic t <$> mutateV t l

-- | Given a 'SolCall', generate a random \"similar\" call with the same 'SolSignature'.
mutateAbiCall :: MonadRandom m => SolCall -> m SolCall
mutateAbiCall = traverse $ traverse mutateAbiValue

-- Generation, with dictionary

-- | Given a generator taking an @a@ and returning a @b@ and a way to get @b@s associated with some
-- @a@ from a GenConf, return a generator that takes an @a@ and either synthesizes new @b@s with the
-- provided generator or uses the 'GenConf' dictionary (when available).
genWithDict :: (Eq a, Hashable a, MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m)
            => (GenConf -> HashMap a [b]) -> (a -> m b) -> a -> m b
genWithDict f g t = asks getter >>= \c -> do
  useD <- (pSynthA c <) <$> getRandom
  g t <**> case (M.lookup t (f c), useD) of (Just l@(_:_), True) -> const <$> rElem "" l
                                            _                    -> pure id

-- | Given an 'AbiType', generate a random 'AbiValue' of that type, possibly with a dictionary.
genAbiValueM :: (MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m) => AbiType -> m AbiValue
genAbiValueM = genWithDict constants genAbiValue

-- | Given a 'SolSignature', generate a random 'SolCalls' with that signature, possibly with a dictionary.
genAbiCallM :: (MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m) => SolSignature -> m SolCall
genAbiCallM = genWithDict wholeCalls genAbiCall

-- | Given a list of 'SolSignature's, generate a random 'SolCall' for one, possibly with a dictionary.
genInteractionsM :: (MonadReader x m, Has GenConf x, MonadRandom m, MonadThrow m)
                 => [SolSignature] -> m SolCall
genInteractionsM l = genAbiCallM =<< rElem "ABI" l
