{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Echidna.ABI where

import Control.Lens
import Control.Monad (join, liftM2, liftM3, foldM, replicateM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.State (evalStateT)
import Control.Monad.Random.Strict (MonadRandom, getRandom, getRandoms, getRandomR, uniformMay)
import Data.Bits (Bits(..))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Has (Has(..))
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet, fromList, union)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector.Instances ()
import Data.Word8 (Word8)
import Numeric (showHex)

import EVM.ABI

import qualified Control.Monad.Random.Strict as R
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Pretty-print some 'AbiValue'.
ppAbiValue :: AbiValue -> String
ppAbiValue (AbiUInt _ n)         = show n
ppAbiValue (AbiInt  _ n)         = show n
ppAbiValue (AbiAddress n)        = showHex n ""
ppAbiValue (AbiBool b)           = if b then "true" else "false"
ppAbiValue (AbiBytes      _ b)   = show b
ppAbiValue (AbiBytesDynamic b)   = show b
ppAbiValue (AbiString       s)   = show s
ppAbiValue (AbiArrayDynamic _ v) =
  "[" ++ intercalate ", " (ppAbiValue <$> toList v) ++ "]"
ppAbiValue (AbiArray      _ _ v) =
  "[" ++ intercalate ", " (ppAbiValue <$> toList v) ++ "]"
ppAbiValue (AbiTuple v) =
  "(" ++ intercalate ", " (ppAbiValue <$> toList v) ++ ")"

-- | Get a random element of a non-empty list.
rElem :: MonadRandom m => NE.NonEmpty a -> m a
rElem l  = (l NE.!!) <$> getRandomR (0, length l - 1)

-- Types

-- Don't construct this directly! Use mkConf.

-- | Represents a call to a Solidity function.
-- A tuple of 'Text' for the name of the function, and then any 'AbiValue' arguments passed (as a list).
type SolCall     = (Text, [AbiValue])

-- | Represents the type of a Solidity function.
-- A tuple of 'Text' for the name of the function, and then the 'AbiType's of any arguments it expects.
type SolSignature = (Text, [AbiType])

-- | Get the text signature of a solidity method (for later hashing)
encodeSig :: SolSignature -> Text
encodeSig (n, ts) = n <> "(" <> T.intercalate "," (abiTypeSolidity <$> ts) <> ")"

-- | Configuration necessary for generating new 'SolCalls'. Don't construct this by hand! Use 'mkConf'.
data GenDict = GenDict { _pSynthA    :: Float
                         -- ^ Fraction of time to use dictionary vs. synthesize
                       , _constants  :: HashMap AbiType (HashSet AbiValue)
                         -- ^ Constants to use, sorted by type
                       , _wholeCalls :: HashMap SolSignature (HashSet SolCall)
                         -- ^ Whole calls to use, sorted by type
                       , _defSeed    :: Int
                         -- ^ Default seed to use if one is not provided in EConfig
                       , _rTypes     :: Text -> Maybe AbiType
                         -- ^ Return types of any methods we scrape return values from
                       }

makeLenses 'GenDict

hashMapBy :: (Hashable k, Hashable a, Eq k, Ord a) => (a -> k) -> [a] -> HashMap k (HashSet a)
hashMapBy f = M.fromListWith union . fmap (\v -> (f v, fromList [v]))

gaddConstants :: [AbiValue] -> GenDict -> GenDict
gaddConstants l = constants <>~ hashMapBy abiValueType l

gaddCalls :: [SolCall] -> GenDict -> GenDict
gaddCalls c = wholeCalls <>~ hashMapBy (fmap $ fmap abiValueType) c

defaultDict :: GenDict
defaultDict = mkGenDict 0 [] [] 0 (const Nothing)

-- This instance is the only way for mkConf to work nicely, and is well-formed.
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}
-- We need the above since hlint doesn't notice DeriveAnyClass in StandaloneDeriving.
deriving instance Hashable AbiType
deriving instance Hashable AbiValue

-- | Construct a 'GenDict' from some dictionaries, a 'Float', a default seed, and a typing rule for
-- return values
mkGenDict :: Float      -- ^ Percentage of time to mutate instead of synthesize. Should be in [0,1]
          -> [AbiValue] -- ^ A list of 'AbiValue' constants to use during dictionary-based generation
          -> [SolCall]  -- ^ A list of complete 'SolCall's to mutate
          -> Int        -- ^ A default seed
          -> (Text -> Maybe AbiType)
          -- ^ A return value typing rule
          -> GenDict
mkGenDict p vs cs = GenDict p (hashMapBy abiValueType vs) (hashMapBy (fmap $ fmap abiValueType) cs)

-- Generation (synthesis)

getRandomUint :: MonadRandom m => Int -> m Integer
getRandomUint n = join $ R.fromList [(getRandomR (0, 1023), 1), (getRandomR (0, 2 ^ n - 1), 9)]

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Doesn't use a dictionary.
-- Note that we define the dictionary case ('genAbiValueM') first (below), so recursive types can be
-- be generated using the same dictionary easily
genAbiValue :: MonadRandom m => AbiType -> m AbiValue
genAbiValue = flip evalStateT defaultDict . genAbiValueM

-- | Synthesize a random 'SolCall' given its 'SolSignature'. Doesn't use a dictionary.
genAbiCall :: MonadRandom m => SolSignature -> m SolCall
genAbiCall = traverse $ traverse genAbiValue

-- | Synthesize a random 'SolCall' given a list of 'SolSignature's (effectively, an ABI). Doesn't use a dictionary.
genInteractions :: (MonadThrow m, MonadRandom m) => NE.NonEmpty SolSignature -> m SolCall
genInteractions l = genAbiCall =<< rElem l

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
canShrinkAbiValue (AbiUInt _ 0)         = False
canShrinkAbiValue (AbiInt  _ 0)         = False
canShrinkAbiValue (AbiBool b)           = b
canShrinkAbiValue (AbiBytes _ b)        = BS.any (/= 0) b
canShrinkAbiValue (AbiBytesDynamic "")  = False
canShrinkAbiValue (AbiString "")        = False
canShrinkAbiValue (AbiArray _ _ l)      = any canShrinkAbiValue l
canShrinkAbiValue (AbiArrayDynamic _ l) = l /= mempty
canShrinkAbiValue (AbiTuple v)          = any canShrinkAbiValue v
canShrinkAbiValue _                     = True

bounds :: forall a. (Bounded a, Integral a) => a -> (Integer, Integer)
bounds = const (fromIntegral (0 :: a), fromIntegral (maxBound :: a))

-- | Given a bitvector representation of an integer type, randomly change bits, shrinking it towards 0
shrinkInt :: forall a m. (Bits a, Bounded a, Integral a, MonadRandom m) => a -> m a
shrinkInt x | x == -1   = pure 0
            | otherwise = (if x < 0 then (.|.) else (.&.)) x . fromIntegral <$> getRandomR (bounds x)

-- | Given an 'AbiValue', generate a random \"smaller\" (simpler) value of the same 'AbiType'.
shrinkAbiValue :: MonadRandom m => AbiValue -> m AbiValue
shrinkAbiValue (AbiUInt n m)         = AbiUInt n <$> shrinkInt m
shrinkAbiValue (AbiInt n m)          = AbiInt n  <$> shrinkInt m
shrinkAbiValue x@AbiAddress{}        = pure x
shrinkAbiValue (AbiBool _)           = pure $ AbiBool False
shrinkAbiValue (AbiBytes n b)        = AbiBytes n <$> addNulls b
shrinkAbiValue (AbiBytesDynamic b)   = fmap AbiBytesDynamic $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiString b)         = fmap AbiString       $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiArray n t l)      = AbiArray n t <$> traverse shrinkAbiValue l
shrinkAbiValue (AbiArrayDynamic t l) = fmap (AbiArrayDynamic t) $ traverse shrinkAbiValue =<< shrinkV l
shrinkAbiValue (AbiTuple v)          = AbiTuple <$> traverse shrinkAbiValue' v
  where shrinkAbiValue' x = liftM3 bool (pure x) (shrinkAbiValue x) getRandom

-- | Given a 'SolCall', generate a random \"smaller\" (simpler) call.
shrinkAbiCall :: MonadRandom m => SolCall -> m SolCall
shrinkAbiCall = traverse $ traverse shrinkAbiValue

-- | Given an 'AbiValue', generate a random \"similar\" value of the same 'AbiType'.
mutateAbiValue :: MonadRandom m => AbiValue -> m AbiValue
mutateAbiValue (AbiUInt n x)         = AbiUInt n         <$> mutateNum x
mutateAbiValue (AbiInt n x)          = AbiInt n          <$> mutateNum x
mutateAbiValue (AbiAddress _)        = genAbiValue AbiAddressType
mutateAbiValue (AbiBool _)           = genAbiValue AbiBoolType
mutateAbiValue (AbiBytes n b)        = AbiBytes n        <$> addChars getRandom b
mutateAbiValue (AbiBytesDynamic b)   = AbiBytesDynamic   <$> mutateBS b
mutateAbiValue (AbiString b)         = AbiString         <$> mutateBS b
mutateAbiValue (AbiArray n t l)      = AbiArray n t      <$> traverse mutateAbiValue l
mutateAbiValue (AbiArrayDynamic t l) = AbiArrayDynamic t <$> mutateV t l
mutateAbiValue (AbiTuple v)          = AbiTuple          <$> traverse mutateAbiValue v

-- | Given a 'SolCall', generate a random \"similar\" call with the same 'SolSignature'.
mutateAbiCall :: MonadRandom m => SolCall -> m SolCall
mutateAbiCall = traverse $ traverse mutateAbiValue

-- Generation, with dictionary

-- | Given a generator taking an @a@ and returning a @b@ and a way to get @b@s associated with some
-- @a@ from a GenDict, return a generator that takes an @a@ and either synthesizes new @b@s with the
-- provided generator or uses the 'GenDict' dictionary (when available).
genWithDict :: (Eq a, Hashable a, MonadState x m, Has GenDict x, MonadRandom m)
            => (GenDict -> HashMap a [b]) -> (a -> m b) -> a -> m b
genWithDict f g t = let fromDict = uniformMay . M.lookupDefault [] t . f in gets getter >>= \c ->
  fromMaybe <$> g t <*> (bool (pure Nothing) (fromDict c) . (c ^. pSynthA >=) =<< getRandom)

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Requires a dictionary.
genAbiValueM :: (MonadState x m, Has GenDict x, MonadRandom m) => AbiType -> m AbiValue
genAbiValueM = genWithDict (fmap toList . view constants) $ \case
  (AbiUIntType n)         -> AbiUInt n  . fromInteger <$> getRandomUint n
  (AbiIntType n)          -> AbiInt n   . fromInteger <$> getRandomR (-1 * 2 ^ n, 2 ^ (n - 1))
  AbiAddressType          -> AbiAddress . fromInteger <$> getRandomR (0, 2 ^ (160 :: Integer) - 1)
  AbiBoolType             -> AbiBool <$> getRandom
  (AbiBytesType n)        -> AbiBytes n . BS.pack . take n <$> getRandoms
  AbiBytesDynamicType     -> liftM2 (\n -> AbiBytesDynamic . BS.pack . take n)
                                    (getRandomR (1, 32)) getRandoms
  AbiStringType           -> liftM2 (\n -> AbiString       . BS.pack . take n)
                                    (getRandomR (1, 32)) getRandoms
  (AbiArrayDynamicType t) -> fmap (AbiArrayDynamic t) $ getRandomR (1, 32)
                             >>= flip V.replicateM (genAbiValueM t)
  (AbiArrayType n t)      -> AbiArray n t <$> V.replicateM n (genAbiValueM t)
  (AbiTupleType v)        -> AbiTuple <$> traverse genAbiValueM v

-- | Given a 'SolSignature', generate a random 'SolCalls' with that signature, possibly with a dictionary.
genAbiCallM :: (MonadState x m, Has GenDict x, MonadRandom m) => SolSignature -> m SolCall
genAbiCallM = genWithDict (fmap toList . view wholeCalls) (traverse $ traverse genAbiValueM)

-- | Given a list of 'SolSignature's, generate a random 'SolCall' for one, possibly with a dictionary.
genInteractionsM :: (MonadState x m, Has GenDict x, MonadRandom m, MonadThrow m)
                 => NE.NonEmpty SolSignature -> m SolCall
genInteractionsM l = genAbiCallM =<< rElem l
