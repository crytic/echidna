{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Echidna.ABI where

import Control.Monad (join, liftM2, liftM3, foldM, replicateM)
import Control.Monad.Random.Strict (MonadRandom, getRandom, getRandoms, getRandomR)
import Control.Monad.Random.Strict qualified as R
import Data.Binary.Put (runPut, putWord32be)
import Data.BinaryWord (unsignedWord)
import Data.Bits (bit)
import Data.Bool (bool)
import Data.ByteString.Lazy as BSLazy (toStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DoubleWord (Int256, Word256)
import Data.Foldable (toList)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Instances ()
import Data.Word (Word8)
import Numeric (showHex)

import EVM.ABI hiding (genAbiValue)
import EVM.Types (Addr, abiKeccak, W256)

import Echidna.Mutator.Array (mutateLL, replaceAt)
import Echidna.Types.Random
import Echidna.Types.Signature

-- | Fallback function is the null string
fallback :: SolSignature
fallback = ("",[])

commonTypeSizes :: [Int]
commonTypeSizes = [8,16..256]

mkValidAbiInt :: Int -> Int256 -> Maybe AbiValue
mkValidAbiInt i x = if unsignedWord (abs x) < bit (i - 1) then Just $ AbiInt i x else Nothing

mkValidAbiUInt :: Int -> Word256 -> Maybe AbiValue
mkValidAbiUInt 256 x = Just $ AbiUInt 256 x
mkValidAbiUInt i x = if x < bit i then Just $ AbiUInt i x else Nothing

makeNumAbiValues :: Integer -> [AbiValue]
makeNumAbiValues i = let l f = f <$> commonTypeSizes <*> fmap fromIntegral ([i-1..i+1] ++ [(-i)-1 .. (-i)+1]) in
    catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)

makeArrayAbiValues :: BS.ByteString -> [AbiValue]
makeArrayAbiValues b = let size = BS.length b in [AbiString b, AbiBytesDynamic b] ++
 fmap (\n -> AbiBytes n . BS.append b $ BS.replicate (n - size) 0) [size..32]

-- | Pretty-print some 'AbiValue'.
ppAbiValue :: AbiValue -> String
ppAbiValue (AbiUInt _ n)         = show n
ppAbiValue (AbiInt  _ n)         = show n
ppAbiValue (AbiAddress n)        = "0x" ++ showHex n ""
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

-- Types

-- Don't construct this directly! Use mkConf.

-- | Get the signature from a Solidity function.
signatureCall :: SolCall -> SolSignature
signatureCall (t, vs) = (t, map abiValueType vs)

-- | Get the text signature of a solidity method (for later hashing)
encodeSig :: SolSignature -> Text
encodeSig (n, ts) = n <> "(" <> T.intercalate "," (abiTypeSolidity <$> ts) <> ")"

-- | Get the text signature of a solidity method (for later hashing)
encodeSigWithName :: Text -> SolSignature -> Text
encodeSigWithName cn (n, ts) = last (T.split (==':') cn) <> "." <> n <> "(" <> T.intercalate "," (abiTypeSolidity <$> ts) <> ")"

-- | Get the signature of a solidity method
hashSig :: Text -> FunctionHash
hashSig = abiKeccak . TE.encodeUtf8

-- | Configuration necessary for generating new 'SolCalls'. Don't construct this by hand! Use 'mkConf'.
data GenDict = GenDict
  { pSynthA    :: Float
    -- ^ Fraction of time to use dictionary vs. synthesize
  , constants  :: HashMap AbiType (Set AbiValue)
    -- ^ Constants to use, sorted by type
  , wholeCalls :: HashMap SolSignature (Set SolCall)
    -- ^ Whole calls to use, sorted by type
  , defSeed    :: Int
    -- ^ Default seed to use if one is not provided in EConfig
  , rTypes     :: Text -> Maybe AbiType
    -- ^ Return types of any methods we scrape return values from
  , dictValues :: Set W256
    -- ^ A set of int/uint constants for better performance
  }

hashMapBy :: (Hashable k, Hashable a, Eq k, Ord a) => (a -> k) -> Set a -> HashMap k (Set a)
hashMapBy f = M.fromListWith Set.union . fmap (\v -> (f v, Set.singleton v)) . Set.toList

gaddCalls :: Set SolCall -> GenDict -> GenDict
gaddCalls calls dict =
  dict { wholeCalls = dict.wholeCalls <> hashMapBy (fmap $ fmap abiValueType) calls }

defaultDict :: GenDict
defaultDict = mkGenDict 0 Set.empty Set.empty 0 (const Nothing)

deriving anyclass instance Hashable AbiType
deriving anyclass instance Hashable AbiValue
deriving anyclass instance Hashable Addr

-- | Construct a 'GenDict' from some dictionaries, a 'Float', a default seed, and a typing rule for
-- return values
mkGenDict :: Float      -- ^ Percentage of time to mutate instead of synthesize. Should be in [0,1]
          -> Set AbiValue -- ^ A list of 'AbiValue' constants to use during dictionary-based generation
          -> Set SolCall  -- ^ A list of complete 'SolCall's to mutate
          -> Int        -- ^ A default seed
          -> (Text -> Maybe AbiType)
          -- ^ A return value typing rule
          -> GenDict
mkGenDict p vs cs s tr =
  GenDict p (hashMapBy abiValueType vs) (hashMapBy (fmap $ fmap abiValueType) cs) s tr (mkDictValues vs)

mkDictValues :: Set AbiValue -> Set W256
mkDictValues = Set.foldl' (\acc e -> maybe acc (`Set.insert` acc) (fromValue e)) Set.empty
  where fromValue (AbiUInt _ n) = Just (fromIntegral n)
        fromValue (AbiInt  _ n) = Just (fromIntegral n)
        fromValue _             = Nothing

-- Generation (synthesis)

getRandomUint :: MonadRandom m => Int -> m Integer
getRandomUint n = join $ R.fromList [(getRandomR (0, 1023), 1), (getRandomR (0, 2 ^ n - 5), 8), (getRandomR (2 ^ n - 5, 2 ^ n - 1), 1)]

getRandomInt :: MonadRandom m => Int -> m Integer
getRandomInt n = join $ R.fromList [(getRandomR (-1023, 1023), 1), (getRandomR (-1 * 2 ^ n, 2 ^ (n - 1)), 9)]

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Doesn't use a dictionary.
-- Note that we define the dictionary case ('genAbiValueM') first (below), so recursive types can be
-- be generated using the same dictionary easily
genAbiValue :: MonadRandom m => AbiType -> m AbiValue
genAbiValue = genAbiValueM defaultDict

-- | Synthesize a random 'SolCall' given its 'SolSignature'. Doesn't use a dictionary.
genAbiCall :: MonadRandom m => SolSignature -> m SolCall
genAbiCall = traverse $ traverse genAbiValue

-- Mutation helper functions

-- | Given an 'Integral' number n, get a random number in [0,2n].
mutateNum :: (Integral a, MonadRandom m) => a -> m a
mutateNum x = bool (x +) (x -) <$> getRandom <*> (fromIntegral <$> getRandomR (0, toInteger x))

fixAbiUInt :: Int -> Word256 -> AbiValue
fixAbiUInt n x = AbiUInt n (x `mod` ((2 ^ n) - 1))

fixAbiInt :: Int -> Int256 -> AbiValue
fixAbiInt n x = if x <= -(2 ^ (n - 1)) then AbiInt n (-(2 ^ n)) else AbiInt n (x `mod` (2 ^ (n - 1) - 1))

-- | Given a way to generate random 'Word8's and a 'ByteString' b of length l,
-- generate between 0 and 2l 'Word8's and add insert them into b at random indices.
addChars :: MonadRandom m => m Word8 -> ByteString -> m ByteString
addChars c b = foldM withR b . enumFromTo 0 =<< rand where
  rand       = getRandomR (0, BS.length b - 1)
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
canShrinkAbiValue (AbiAddress 0)        = False
canShrinkAbiValue _                     = True

shrinkInt :: (Integral a, MonadRandom m) => a -> m a
shrinkInt x = fromIntegral <$> getRandomR (0, toInteger x)

-- | Given an 'AbiValue', generate a random \"smaller\" (simpler) value of the same 'AbiType'.
shrinkAbiValue :: MonadRandom m => AbiValue -> m AbiValue
shrinkAbiValue (AbiUInt n m)         = AbiUInt n <$> shrinkInt m
shrinkAbiValue (AbiInt n m)          = AbiInt n  <$> shrinkInt m
shrinkAbiValue (AbiAddress 0)        = pure $ AbiAddress 0
shrinkAbiValue (AbiAddress _)        = rElem' $ Set.fromList [AbiAddress 0, AbiAddress 0xdeadbeef]
shrinkAbiValue (AbiBool _)           = pure $ AbiBool False
shrinkAbiValue (AbiBytes n b)        = AbiBytes n <$> addNulls b
shrinkAbiValue (AbiBytesDynamic b)   = fmap AbiBytesDynamic $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiString b)         = fmap AbiString       $ addNulls =<< shrinkBS b
shrinkAbiValue (AbiArray n t l)      = AbiArray n t <$> traverse shrinkAbiValue l
shrinkAbiValue (AbiArrayDynamic t l) = getRandomR (0, 9 :: Int) >>= -- 10% of chance of shrinking all elements
                                          \case
                                            0 -> AbiArrayDynamic t <$> traverse shrinkAbiValue l
                                            _ -> AbiArrayDynamic t <$> shrinkV l
shrinkAbiValue (AbiTuple v)          = AbiTuple <$> traverse shrinkAbiValue' v
  where shrinkAbiValue' x = liftM3 bool (pure x) (shrinkAbiValue x) getRandom

-- | Given a 'SolCall', generate a random \"smaller\" (simpler) call.
shrinkAbiCall :: MonadRandom m => SolCall -> m SolCall
shrinkAbiCall = traverse $ traverse shrinkAbiValue

-- | Given an 'AbiValue', generate a random \"similar\" value of the same 'AbiType'.
mutateAbiValue :: MonadRandom m => AbiValue -> m AbiValue
mutateAbiValue (AbiUInt n x)         = getRandomR (0, 9 :: Int) >>= -- 10% of chance of mutation
                                          \case
                                            0 -> fixAbiUInt n <$> mutateNum x
                                            _ -> return $ AbiUInt n x
mutateAbiValue (AbiInt n x)          = getRandomR (0, 9 :: Int) >>= -- 10% of chance of mutation
                                          \case
                                            0 -> fixAbiInt n <$> mutateNum x
                                            _ -> return $ AbiInt n x

mutateAbiValue (AbiAddress x)        = return $ AbiAddress x
mutateAbiValue (AbiBool _)           = genAbiValue AbiBoolType
mutateAbiValue (AbiBytes n b)        = do fs <- replicateM n getRandom
                                          xs <- mutateLL (Just n) (BS.pack fs) b
                                          return (AbiBytes n xs)

mutateAbiValue (AbiBytesDynamic b)   = AbiBytesDynamic <$> mutateLL Nothing mempty b
mutateAbiValue (AbiString b)         = AbiString <$> mutateLL Nothing mempty b
mutateAbiValue (AbiArray n t l)      = do fs <- replicateM n $ genAbiValue t
                                          xs <- mutateLL (Just n) (V.fromList fs) l
                                          return (AbiArray n t xs)

mutateAbiValue (AbiArrayDynamic t l) = AbiArrayDynamic t <$> mutateLL Nothing mempty l
mutateAbiValue (AbiTuple v)          = AbiTuple          <$> traverse mutateAbiValue v

-- | Given a 'SolCall', generate a random \"similar\" call with the same 'SolSignature'.
mutateAbiCall :: (MonadRandom m) => SolCall -> m SolCall
mutateAbiCall = traverse f
                where f  [] = return []
                      f  xs = do k <- getRandomR (0, length xs - 1)
                                 mv <- mutateAbiValue $ xs !! k
                                 return $ replaceAt mv xs k

-- Generation, with dictionary

-- | Given a generator taking an @a@ and returning a @b@ and a way to get @b@s associated with some
-- @a@ from a GenDict, return a generator that takes an @a@ and either synthesizes new @b@s with the
-- provided generator or uses the 'GenDict' dictionary (when available).
genWithDict :: (Eq a, Hashable a, MonadRandom m)
            => GenDict -> HashMap a (Set b) -> (a -> m b) -> a -> m b
genWithDict genDict m g t = do
  r <- getRandom
  let maybeValM = if genDict.pSynthA >= r then fromDict else pure Nothing
      fromDict = case M.lookup t m of
                   Nothing -> pure Nothing
                   Just cs -> Just <$> rElem' cs
  fromMaybe <$> g t <*> maybeValM

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Requires a dictionary.
genAbiValueM :: MonadRandom m => GenDict -> AbiType -> m AbiValue
genAbiValueM genDict = genWithDict genDict genDict.constants $ \case
  (AbiUIntType n)         -> fixAbiUInt n . fromInteger <$> getRandomUint n
  (AbiIntType n)          -> fixAbiInt n . fromInteger <$> getRandomInt n
  AbiAddressType          -> AbiAddress . fromInteger <$> getRandomR (0, 2 ^ (160 :: Integer) - 1)
  AbiBoolType             -> AbiBool <$> getRandom
  (AbiBytesType n)        -> AbiBytes n . BS.pack . take n <$> getRandoms
  AbiBytesDynamicType     -> liftM2 (\n -> AbiBytesDynamic . BS.pack . take n)
                                    (getRandomR (1, 32)) getRandoms
  AbiStringType           -> liftM2 (\n -> AbiString       . BS.pack . take n)
                                    (getRandomR (1, 32)) getRandoms
  (AbiArrayDynamicType t) -> fmap (AbiArrayDynamic t) $ getRandomR (1, 32)
                             >>= flip V.replicateM (genAbiValueM genDict t)
  (AbiArrayType n t)      -> AbiArray n t <$> V.replicateM n (genAbiValueM genDict t)
  (AbiTupleType v)        -> AbiTuple <$> traverse (genAbiValueM genDict) v

-- | Given a 'SolSignature', generate a random 'SolCalls' with that signature, possibly with a dictionary.
genAbiCallM :: MonadRandom m => GenDict -> SolSignature -> m SolCall
genAbiCallM genDict abi = do
  solCall <- genWithDict genDict
                         genDict.wholeCalls
                         (traverse $ traverse (genAbiValueM genDict))
                         abi
  mutateAbiCall solCall

-- | Given a list of 'SolSignature's, generate a random 'SolCall' for one, possibly with a dictionary.
genInteractionsM :: MonadRandom m => GenDict -> NE.NonEmpty SolSignature -> m SolCall
genInteractionsM genDict l = genAbiCallM genDict =<< rElem l

abiCalldata :: Text -> Vector AbiValue -> BS.ByteString
abiCalldata s xs = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s))
  putAbi (AbiTuple xs)
