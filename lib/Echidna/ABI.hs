{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.ABI where

import Control.Lens
import Control.Monad.Catch (Exception, MonadThrow(..))
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State (evalStateT)
import Control.Monad.Random.Strict
import Data.Bits (Bits(..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.DoubleWord (Word256, Int256, Word160)
import Data.Foldable (toList)
import Data.Has (Has(..))
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.List (group, intercalate, sort)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word8 (Word8)
import GHC.Generics (Generic)
import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

-- types for EVM ABIv2
data AbiValue2 = AbiUInt2         Int Word256
               | AbiInt2          Int Int256
               | AbiAddress2      Word160
               | AbiBool2         Bool
               | AbiBytes2        Int BS.ByteString
               | AbiBytesDynamic2 BS.ByteString
               | AbiString2       BS.ByteString
               | AbiArrayDynamic2 AbiType2 (Vector AbiValue2)
               | AbiArray2        Int AbiType2 (Vector AbiValue2)
               | AbiTuple2        (Vector AbiValue2)
  deriving (Show, Read, Eq, Ord, Generic)

data AbiType2 = AbiUIntType2         Int
              | AbiIntType2          Int
              | AbiAddressType2
              | AbiBoolType2
              | AbiBytesType2        Int
              | AbiBytesDynamicType2
              | AbiStringType2
              | AbiArrayDynamicType2 AbiType2
              | AbiArrayType2        Int AbiType2
              | AbiTupleType2        (Vector AbiType2)
  deriving (Show, Read, Eq, Ord, Generic)

instance Hashable a => Hashable (Vector a) where
  hashWithSalt s = hashWithSalt s . V.toList

abiValueType2 :: AbiValue2 -> AbiType2
abiValueType2 = \case
  AbiUInt2 n _         -> AbiUIntType2 n
  AbiInt2 n _          -> AbiIntType2  n
  AbiAddress2 _        -> AbiAddressType2
  AbiBool2 _           -> AbiBoolType2
  AbiBytes2 n _        -> AbiBytesType2 n
  AbiBytesDynamic2 _   -> AbiBytesDynamicType2
  AbiString2 _         -> AbiStringType2
  AbiArrayDynamic2 t _ -> AbiArrayDynamicType2 t
  AbiArray2 n t _      -> AbiArrayType2 n t
  AbiTuple2 v          -> AbiTupleType2 (abiValueType2 <$> v)

abiTypeSolidity2 :: AbiType2 -> Text
abiTypeSolidity2 = \case
  AbiUIntType2 n         -> "uint" <> T.pack (show n)
  AbiIntType2 n          -> "int" <> T.pack (show n)
  AbiAddressType2        -> "address"
  AbiBoolType2           -> "bool"
  AbiBytesType2 n        -> "bytes" <> T.pack (show n)
  AbiBytesDynamicType2   -> "bytes"
  AbiStringType2         -> "string"
  AbiArrayDynamicType2 t -> abiTypeSolidity2 t <> "[]"
  AbiArrayType2 n t      -> abiTypeSolidity2 t <> "[" <> T.pack (show n) <> "]"
  AbiTupleType2 v        -> "(" <> (T.intercalate "," . toList $ abiTypeSolidity2 <$> v) <> ")"

data AbiKind = Dynamic | Static
  deriving (Show, Read, Eq, Ord)

abiKind2 :: AbiType2 -> AbiKind
abiKind2 = \case
  AbiBytesDynamicType2   -> Dynamic
  AbiStringType2         -> Dynamic
  AbiArrayDynamicType2 _ -> Dynamic
  AbiArrayType2 _ t      -> abiKind2 t
  AbiTupleType2 v        -> if any (==Dynamic) (abiKind2 <$> v) then Dynamic else Static
  _                      -> Static

-- | Pretty-print some 'AbiValue'.
ppAbiValue2 :: AbiValue2 -> String
ppAbiValue2 (AbiUInt2 _ n)         = show n
ppAbiValue2 (AbiInt2  _ n)         = show n
ppAbiValue2 (AbiAddress2 n)        = showHex n ""
ppAbiValue2 (AbiBool2 b)           = if b then "true" else "false"
ppAbiValue2 (AbiBytes2      _ b)   = show b
ppAbiValue2 (AbiBytesDynamic2 b)   = show b
ppAbiValue2 (AbiString2       s)   = show s
ppAbiValue2 (AbiArrayDynamic2 _ v) =
  "[" ++ intercalate ", " (ppAbiValue2 <$> toList v) ++ "]"
ppAbiValue2 (AbiArray2      _ _ v) =
  "[" ++ intercalate ", " (ppAbiValue2 <$> toList v) ++ "]"
ppAbiValue2 (AbiTuple2 v) =
  "(" ++ intercalate ", " (ppAbiValue2 <$> toList v) ++ ")"

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
type SolCall2     = (Text, [AbiValue2])

-- | Represents the type of a Solidity function.
-- A tuple of 'Text' for the name of the function, and then the 'AbiType's of any arguments it expects.
type SolSignature2 = (Text, [AbiType2])

-- | Get the text signature of a solidity method (for later hashing)
encodeSig2 :: SolSignature2 -> Text
encodeSig2 (n, ts) = n <> "(" <> T.intercalate "," (abiTypeSolidity2 <$> ts) <> ")"

-- | Configuration necessary for generating new 'SolCalls'. Don't construct this by hand! Use 'mkConf'.
data GenDict2 = GenDict2 { _pSynthA2    :: Float
                           -- ^ Fraction of time to use dictionary vs. synthesize
                         , _constants2  :: HashMap AbiType2 [AbiValue2]
                           -- ^ Constants to use, sorted by type
                         , _wholeCalls2 :: HashMap SolSignature2 [SolCall2]
                           -- ^ Whole calls to use, sorted by type
                         , _defSeed2    :: Int
                           -- ^ Default seed to use if one is not provided in EConfig
                         , _rTypes2     :: Text -> Maybe AbiType2
                           -- ^ Return types of any methods we scrape return values from
                         }

makeLenses 'GenDict2

hashMapBy :: (Hashable k, Eq k, Ord a) => (a -> k) -> [a] -> HashMap k [a]
hashMapBy f = M.fromListWith (++) . mapMaybe (liftM2 fmap (\l x -> (f x, l)) listToMaybe) . group . sort

gaddConstants2 :: [AbiValue2] -> GenDict2 -> GenDict2
gaddConstants2 l = constants2 <>~ hashMapBy abiValueType2 l

gaddCalls2 :: [SolCall2] -> GenDict2 -> GenDict2
gaddCalls2 c = wholeCalls2 <>~ hashMapBy (fmap $ fmap abiValueType2) c

defaultDict2 :: GenDict2
defaultDict2 = mkGenDict2 0 [] [] 0 (const Nothing)

-- This instance is the only way for mkConf to work nicely, and is well-formed.
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}
-- We need the above since hlint doesn't notice DeriveAnyClass in StandaloneDeriving.
deriving instance Hashable AbiType2

-- | Construct a 'GenDict' from some dictionaries, a 'Float', a default seed, and a typing rule for
-- return values
mkGenDict2 :: Float       -- ^ Percentage of time to mutate instead of synthesize. Should be in [0,1]
           -> [AbiValue2] -- ^ A list of 'AbiValue' constants to use during dictionary-based generation
           -> [SolCall2]  -- ^ A list of complete 'SolCall's to mutate
           -> Int         -- ^ A default seed
           -> (Text -> Maybe AbiType2)
           -- ^ A return value typing rule
           -> GenDict2
mkGenDict2 p vs cs = GenDict2 p (hashMapBy abiValueType2 vs) (hashMapBy (fmap $ fmap abiValueType2) cs)

-- Generation (synthesis)

getRandomUint :: MonadRandom m => Int -> m Integer
getRandomUint n = join $ fromList [(getRandomR (0, 1023), 1), (getRandomR (0, 2 ^ n - 1), 9)]

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Doesn't use a dictionary.
-- Note that we define the dictionary case ('genAbiValueM') first (below), so recursive types can be
-- be generated using the same dictionary easily
genAbiValue2 :: MonadRandom m => AbiType2 -> m AbiValue2
genAbiValue2 = flip evalStateT defaultDict2 . genAbiValueM2

-- | Synthesize a random 'SolCall' given its 'SolSignature'. Doesn't use a dictionary.
genAbiCall2 :: MonadRandom m => SolSignature2 -> m SolCall2
genAbiCall2 = traverse $ traverse genAbiValue2

-- | Synthesize a random 'SolCall' given a list of 'SolSignature's (effectively, an ABI). Doesn't use a dictionary.
genInteractions2 :: (MonadThrow m, MonadRandom m) => [SolSignature2] -> m SolCall2
genInteractions2 l = genAbiCall2 =<< rElem "ABIv2" l

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
mutateV2 :: MonadRandom m => AbiType2 -> Vector AbiValue2 -> m (Vector AbiValue2)
mutateV2 t v = traverse mutateAbiValue2 =<< changeSize where
  changeSize = bool (shrinkV v) (growWith (genAbiValue2 t) V.cons V.snoc V.length v) =<< getRandom

-- Mutation

-- | Check if an 'AbiValue' is as \"small\" (trivial) as possible (using ad-hoc heuristics).
canShrinkAbiValue2 :: AbiValue2 -> Bool
canShrinkAbiValue2 (AbiUInt2 _ 0) = False
canShrinkAbiValue2 (AbiInt2  _ 0) = False
canShrinkAbiValue2 (AbiBool2 b) = b
canShrinkAbiValue2 (AbiBytes2 _ b)       = BS.any (/= 0) b
canShrinkAbiValue2 (AbiBytesDynamic2 "") = False
canShrinkAbiValue2 (AbiString2 "")       = False
canShrinkAbiValue2 (AbiArray2 _ _ l)      = any canShrinkAbiValue2 l
canShrinkAbiValue2 (AbiArrayDynamic2 _ l) = l /= mempty
canShrinkAbiValue2 (AbiTuple2 v) = any canShrinkAbiValue2 v
canShrinkAbiValue2 _ = True

bounds :: forall a. (Bounded a, Integral a) => a -> (Integer, Integer)
bounds = const (fromIntegral (0 :: a), fromIntegral (maxBound :: a))

-- | Given a bitvector representation of an integer type, randomly change bits, shrinking it towards 0
shrinkInt :: forall a m. (Bits a, Bounded a, Integral a, MonadRandom m) => a -> m a
shrinkInt x | x == -1   = pure 0
            | otherwise = (if x < 0 then (.|.) else (.&.)) x . fromIntegral <$> getRandomR (bounds x)

-- | Given an 'AbiValue', generate a random \"smaller\" (simpler) value of the same 'AbiType'.
shrinkAbiValue2 :: MonadRandom m => AbiValue2 -> m AbiValue2
shrinkAbiValue2 (AbiUInt2 n m)  = AbiUInt2 n <$> shrinkInt m
shrinkAbiValue2 (AbiInt2 n m)   = AbiInt2 n  <$> shrinkInt m
shrinkAbiValue2 x@AbiAddress2{} = pure x
shrinkAbiValue2 (AbiBool2 _)    = pure $ AbiBool2 False
shrinkAbiValue2 (AbiBytes2 n b)      = AbiBytes2 n <$> addNulls b
shrinkAbiValue2 (AbiBytesDynamic2 b) = fmap AbiBytesDynamic2 $ addNulls =<< shrinkBS b
shrinkAbiValue2 (AbiString2 b)       = fmap AbiString2       $ addNulls =<< shrinkBS b
shrinkAbiValue2 (AbiArray2 n t l)    = AbiArray2 n t <$> traverse shrinkAbiValue2 l
shrinkAbiValue2 (AbiArrayDynamic2 t l) = fmap (AbiArrayDynamic2 t) $ traverse shrinkAbiValue2 =<< shrinkV l
shrinkAbiValue2 (AbiTuple2 v)   = AbiTuple2 <$> traverse shrinkAbiValue2' v
  where shrinkAbiValue2' x = do
          f <- uniform [return . id, shrinkAbiValue2]
          f x

-- | Given a 'SolCall', generate a random \"smaller\" (simpler) call.
shrinkAbiCall2 :: MonadRandom m => SolCall2 -> m SolCall2
shrinkAbiCall2 = traverse $ traverse shrinkAbiValue2

-- | Given an 'AbiValue', generate a random \"similar\" value of the same 'AbiType'.
mutateAbiValue2 :: MonadRandom m => AbiValue2 -> m AbiValue2
mutateAbiValue2 (AbiUInt2 n x)  = AbiUInt2 n <$> mutateNum x
mutateAbiValue2 (AbiInt2 n x)   = AbiInt2 n  <$> mutateNum x
mutateAbiValue2 (AbiAddress2 _) = genAbiValue2 AbiAddressType2
mutateAbiValue2 (AbiBool2 _)    = genAbiValue2 AbiBoolType2
mutateAbiValue2 (AbiBytes2 n b)        = AbiBytes2 n        <$> addChars getRandom b
mutateAbiValue2 (AbiBytesDynamic2 b)   = AbiBytesDynamic2   <$> mutateBS b
mutateAbiValue2 (AbiString2 b)         = AbiString2         <$> mutateBS b
mutateAbiValue2 (AbiArray2 n t l)      = AbiArray2 n t      <$> traverse mutateAbiValue2 l
mutateAbiValue2 (AbiArrayDynamic2 t l) = AbiArrayDynamic2 t <$> mutateV2 t l
mutateAbiValue2 (AbiTuple2 v)  = AbiTuple2 <$> traverse mutateAbiValue2 v

-- | Given a 'SolCall', generate a random \"similar\" call with the same 'SolSignature'.
mutateAbiCall2 :: MonadRandom m => SolCall2 -> m SolCall2
mutateAbiCall2 = traverse $ traverse mutateAbiValue2

-- Generation, with dictionary

-- | Given a generator taking an @a@ and returning a @b@ and a way to get @b@s associated with some
-- @a@ from a GenDict, return a generator that takes an @a@ and either synthesizes new @b@s with the
-- provided generator or uses the 'GenDict' dictionary (when available).
genWithDict2 :: (Eq a, Hashable a, MonadState x m, Has GenDict2 x, MonadRandom m)
             => (GenDict2 -> HashMap a [b]) -> (a -> m b) -> a -> m b
genWithDict2 f g t = let fromDict = uniformMay . M.lookupDefault [] t . f in gets getter >>= \c ->
  fromMaybe <$> g t <*> (bool (pure Nothing) (fromDict c) . (c ^. pSynthA2 >=) =<< getRandom)

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Requires a dictionary.
genAbiValueM2 :: (MonadState x m, Has GenDict2 x, MonadRandom m) => AbiType2 -> m AbiValue2
genAbiValueM2 = genWithDict2 (view constants2) $ \case
  (AbiUIntType2 n) -> AbiUInt2 n  . fromInteger <$> getRandomUint n
  (AbiIntType2 n)  -> AbiInt2 n   . fromInteger <$> getRandomR (-1 * 2 ^ n, 2 ^ (n - 1))
  AbiAddressType2  -> AbiAddress2 . fromInteger <$> getRandomR (0, 2 ^ (160 :: Integer) - 1)
  AbiBoolType2     -> AbiBool2 <$> getRandom
  (AbiBytesType2 n)    -> AbiBytes2 n . BS.pack . take n <$> getRandoms
  AbiBytesDynamicType2 -> liftM2 (\n -> AbiBytesDynamic2 . BS.pack . take n)
                                 (getRandomR (1, 32)) getRandoms
  AbiStringType2       -> liftM2 (\n -> AbiString2       . BS.pack . take n)
                                 (getRandomR (1, 32)) getRandoms
  (AbiArrayDynamicType2 t) -> fmap (AbiArrayDynamic2 t) $ getRandomR (1, 32)
                              >>= flip V.replicateM (genAbiValueM2 t)
  (AbiArrayType2 n t)      -> AbiArray2 n t <$> V.replicateM n (genAbiValueM2 t)
  (AbiTupleType2 v)    -> AbiTuple2 <$> traverse genAbiValueM2 v

-- | Given a 'SolSignature', generate a random 'SolCalls' with that signature, possibly with a dictionary.
genAbiCallM2 :: (MonadState x m, Has GenDict2 x, MonadRandom m) => SolSignature2 -> m SolCall2
genAbiCallM2 = genWithDict2 (view wholeCalls2) (traverse $ traverse genAbiValueM2)

-- | Given a list of 'SolSignature's, generate a random 'SolCall' for one, possibly with a dictionary.
genInteractionsM2 :: (MonadState x m, Has GenDict2 x, MonadRandom m, MonadThrow m)
                  => [SolSignature2] -> m SolCall2
genInteractionsM2 l = genAbiCallM2 =<< rElem "ABI" l
