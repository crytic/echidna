module Echidna.ABI where

import Control.Monad (liftM2, liftM3, foldM, replicateM)
import Control.Monad.Random.Strict (MonadRandom, join, getRandom, getRandoms, getRandomR, uniform, fromList)
import Control.Monad.Random.Strict qualified as Random
import Data.Binary.Put (runPut, putWord32be)
import Data.BinaryWord (unsignedWord)
import Data.Bits (bit)
import Data.Bool (bool)
import Data.ByteString.Lazy as BSLazy (toStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DoubleWord (Int256, Word256)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8)
import Numeric (showHex)

import EVM.ABI hiding (genAbiValue)
import EVM.Types (Addr, abiKeccak, W256, FunctionSelector(..))

import Echidna.Mutator.Array (mutateLL, replaceAt)
import Echidna.Types.Random
import Echidna.Types.Signature

-- | Fallback function is the null string
fallback :: SolSignature
fallback = ("",[])

commonTypeSizes :: [Int]
commonTypeSizes = [8,16..256]

mkValidAbiInt :: Int -> Int256 -> Maybe AbiValue
mkValidAbiInt i x =
  if unsignedWord (abs x) < bit (i - 1)
     then Just $ AbiInt i x
     else Nothing

mkValidAbiUInt :: Int -> Word256 -> Maybe AbiValue
mkValidAbiUInt 256 x = Just $ AbiUInt 256 x
mkValidAbiUInt i x = if x < bit i then Just $ AbiUInt i x else Nothing

makeNumAbiValues :: Integer -> [AbiValue]
makeNumAbiValues i =
  let l f = f <$> commonTypeSizes <*> fmap fromIntegral ([i-1..i+1] ++ [(-i)-1 .. (-i)+1])
  in catMaybes (l mkValidAbiInt ++ l mkValidAbiUInt)

makeArrayAbiValues :: ByteString -> [AbiValue]
makeArrayAbiValues b =
  let size = BS.length b
  in [AbiString b, AbiBytesDynamic b] ++
     fmap (\n -> AbiBytes n . BS.append b $ BS.replicate (n - size) 0) [size..32]

-- | Pretty-print some 'AbiValue'.
ppAbiValue :: AbiValue -> String
ppAbiValue = \case
  AbiUInt _ n         -> show n
  AbiInt  _ n         -> show n
  AbiAddress n        -> "0x" <> showHex n ""
  AbiBool b           -> if b then "true" else "false"
  AbiBytes _ b        -> show b
  AbiBytesDynamic b   -> show b
  AbiString s         -> show s
  AbiArrayDynamic _ v -> "[" <> commaSeparated v <> "]"
  AbiArray _ _ v      -> "[" <> commaSeparated v <> "]"
  AbiTuple v          -> "(" <> commaSeparated v <> ")"
  AbiFunction v       -> show v
  where commaSeparated v = intercalate ", " (ppAbiValue <$> toList v)

-- | Get the signature from a Solidity function.
signatureCall :: SolCall -> SolSignature
signatureCall (t, vs) = (t, map abiValueType vs)

-- | Get the text signature of a solidity method (for later hashing)
encodeSig :: SolSignature -> Text
encodeSig (n, ts) =
  n <> "(" <> T.intercalate "," (abiTypeSolidity <$> ts) <> ")"

-- | Get the text signature of a solidity method (for later hashing)
encodeSigWithName :: Text -> SolSignature -> Text
encodeSigWithName cn (n, ts) =
  last (T.split (==':') cn) <> "." <> n <> "(" <> T.intercalate "," (abiTypeSolidity <$> ts) <> ")"

-- | Get the signature of a solidity method
hashSig :: Text -> FunctionSelector
hashSig = abiKeccak . TE.encodeUtf8

-- | Configuration necessary for generating new 'SolCall's. Don't construct this
-- by hand! Use 'mkGenDict'.
data GenDict = GenDict
  { pSynthA    :: Float
    -- ^ Fraction of time to use dictionary vs. synthesize
  , constants  :: !(Map AbiType (Set AbiValue))
    -- ^ Constants to use, sorted by type
  , wholeCalls :: !(Map SolSignature (Set SolCall))
    -- ^ Whole calls to use, sorted by type
  , defSeed    :: Int
    -- ^ Default seed to use if one is not provided in EConfig
  , rTypes     :: Text -> Maybe AbiType
    -- ^ Return types of any methods we scrape return values from
  , dictValues :: !(Set W256)
    -- ^ A set of int/uint constants for better performance
  }

hashMapBy
  :: (Ord k, Eq k, Ord a)
  => (a -> k)
  -> Set a
  -> Map k (Set a)
hashMapBy f = Map.fromListWith Set.union . fmap (\v -> (f v, Set.singleton v)) . Set.toList

gaddCalls :: Set SolCall -> GenDict -> GenDict
gaddCalls calls dict =
  dict { wholeCalls = dict.wholeCalls <> hashMapBy (fmap $ fmap abiValueType) calls }

-- | Construct a 'GenDict' from some dictionaries, a 'Float', a default seed,
-- and a typing rule for return values
mkGenDict
  :: Float        -- ^ Percentage of time to mutate instead of synthesize. Should be in [0,1]
  -> Set AbiValue -- ^ A list of 'AbiValue' constants to use during dictionary-based generation
  -> Set SolCall  -- ^ A list of complete 'SolCall's to mutate
  -> Int          -- ^ A default seed
  -> (Text -> Maybe AbiType) -- ^ A return value typing rule
  -> GenDict
mkGenDict mutationChance abiValues solCalls seed typingRule =
  GenDict mutationChance
          (hashMapBy abiValueType abiValues)
          (hashMapBy (fmap $ fmap abiValueType) solCalls)
          seed
          typingRule
          (mkDictValues abiValues)

emptyDict :: GenDict
emptyDict = mkGenDict 0 Set.empty Set.empty 0 (const Nothing)

mkDictValues :: Set AbiValue -> Set W256
mkDictValues =
  Set.foldl' (\acc e -> maybe acc (`Set.insert` acc) (fromValue e)) Set.empty
  where fromValue (AbiUInt _ n) = Just (fromIntegral n)
        fromValue (AbiInt  _ n) = Just (fromIntegral n)
        fromValue _             = Nothing

-- Generate a random integer using a pow scale:
getRandomPow :: (MonadRandom m) => Int -> m Integer
getRandomPow n = if n <= 0 then return 0 else
  do
   -- generate uniformly a number from 20 to n
   mexp <- getRandomR (20, n)
   -- generate uniformly a number from the range 2 ^ (mexp / 2) to 2 ^ mexp  
   getRandomR (2 ^ (mexp `div` 2), 2 ^ mexp)

-- Generate a random unsigned integer with the following distribution:
-- * 9% (2/21) uniformly from 0 to 1024
-- * 76% (16/21) uniformly from 0 to 2 ^ n - 5
-- * 9% (2/21) uniformly from 2 ^ n - 5 to 2 ^ n - 1.
-- * 4% (1/21) using the getRandomPow function  
getRandomUint :: MonadRandom m => Int -> m Integer
getRandomUint n =
  join $ Random.weighted
    [ (getRandomR (0, 1023), 2)
    , (getRandomR (0, 2 ^ n - 5), 16)
    , (getRandomR (2 ^ n - 5, 2 ^ n - 1), 2)
    , (getRandomPow (n - 5), 1)
    ]

-- | Generate a random signed integer with the following distribution:
-- * 10% uniformly from the range -1023 to 1023.
-- * 90% uniformly from the range -1 * 2 ^ n to 2 ^ (n - 1). 
getRandomInt :: MonadRandom m => Int -> m Integer
getRandomInt n =
  getRandomR =<< Random.weighted
    [ ((-1023, 1023), 1)
    , ((-1 * 2 ^ n, 2 ^ (n - 1)), 9)
    ]

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Doesn't use a dictionary.
-- Note that we define the dictionary case ('genAbiValueM') first (below), so
-- recursive types can be generated using the same dictionary easily
genAbiValue :: MonadRandom m => AbiType -> m AbiValue
genAbiValue = genAbiValueM emptyDict

-- Mutation helper functions

-- | Given an 'Integral' number n, get a random number in [0,2n].
mutateNum :: (Integral a, MonadRandom m) => a -> m a
mutateNum x =
  bool (x +) (x -) <$> getRandom
                   <*> (fromIntegral <$> getRandomR (0, toInteger x))

fixAbiUInt :: Int -> Word256 -> AbiValue
fixAbiUInt n x = AbiUInt n (x `mod` ((2 ^ n) - 1))

fixAbiInt :: Int -> Int256 -> AbiValue
fixAbiInt n x =
  if x <= -(2 ^ (n - 1))
     then AbiInt n (-(2 ^ n))
     else AbiInt n (x `mod` (2 ^ (n - 1) - 1))

-- | Given a way to generate random 'Word8's and a 'ByteString' b of length l,
-- generate between 0 and 2l 'Word8's and add insert them into b at random indices.
addChars :: MonadRandom m => m Word8 -> ByteString -> m ByteString
addChars c b = foldM withR b . enumFromTo 0 =<< rand where
  rand       = getRandomR (0, BS.length b - 1)
  withR b' n = (\x -> BS.take n b' <> BS.singleton x <> BS.drop (n + 1) b') <$> c

-- | Like 'addChars', but instead of adding random chars, simply adding null bytes.
addNulls :: MonadRandom m => ByteString -> m ByteString
addNulls = addChars $ pure 0

-- | Given a \"list-y\" structure with analogs of 'take', 'drop', and 'length',
-- remove some elements at random.
shrinkWith
  :: MonadRandom m
  => (Int -> a -> a) -- ^ 'take' function
  -> (Int -> a -> a) -- ^ 'drop' function
  -> (a -> Int)      -- ^ 'length' function
  -> a               -- ^ Structure to shrink
  -> m a
shrinkWith f g l t =
  let rand = getRandomR (0, l t)
  in liftM2 (\x y -> f x $ g y t) rand rand

-- | Given a 'ByteString', remove some characters at random.
shrinkBS :: MonadRandom m => ByteString -> m ByteString
shrinkBS = shrinkWith BS.take BS.drop BS.length

-- | Given a 'Vector', remove some elements at random.
shrinkV :: MonadRandom m => Vector a -> m (Vector a)
shrinkV = shrinkWith V.take V.drop V.length

-- Mutation

-- | Check if an 'AbiValue' is as \"small\" (trivial) as possible (using ad-hoc heuristics).
canShrinkAbiValue :: AbiValue -> Bool
canShrinkAbiValue = \case
  AbiUInt _ 0         -> False
  AbiInt  _ 0         -> False
  AbiBool b           -> b
  AbiBytes _ b        -> BS.any (/= 0) b
  AbiBytesDynamic ""  -> False
  AbiString ""        -> False
  AbiArray _ _ l      -> any canShrinkAbiValue l
  AbiArrayDynamic _ l -> l /= mempty
  AbiTuple v          -> any canShrinkAbiValue v
  AbiAddress 0        -> False
  _                   -> True

shrinkInt :: (Integral a, MonadRandom m) => a -> m a
shrinkInt x = fromIntegral <$> getRandomR (0, toInteger x)

-- | Given an 'AbiValue', generate a random \"smaller\" (simpler) value of the same 'AbiType'.
shrinkAbiValue :: MonadRandom m => AbiValue -> m AbiValue
shrinkAbiValue = \case
  AbiUInt n m         -> AbiUInt n <$> shrinkInt m
  AbiInt n m          -> AbiInt n  <$> shrinkInt m
  AbiAddress 0        -> pure $ AbiAddress 0
  AbiAddress _        -> rElem' $ Set.fromList [AbiAddress 0, AbiAddress 0xdeadbeef]
  AbiBool _           -> pure $ AbiBool False
  AbiBytes n b        -> AbiBytes n <$> addNulls b
  AbiBytesDynamic b   -> fmap AbiBytesDynamic $ addNulls =<< shrinkBS b
  AbiString b         -> fmap AbiString       $ addNulls =<< shrinkBS b
  AbiArray n t l      -> AbiArray n t <$> traverse shrinkAbiValue l
  AbiArrayDynamic t l -> getRandomR (0, 9 :: Int) >>=
    -- 10% of chance of shrinking all elements
    \case 0 -> AbiArrayDynamic t <$> traverse shrinkAbiValue l
          _ -> AbiArrayDynamic t <$> shrinkV l
  AbiTuple v          -> AbiTuple <$> traverse shrinkAbiValue' v
  AbiFunction v       -> pure $ AbiFunction v
  where shrinkAbiValue' x = liftM3 bool (pure x) (shrinkAbiValue x) getRandom

-- | Given a 'SolCall', generate a random \"smaller\" (simpler) call.
shrinkAbiCall :: MonadRandom m => SolCall -> m SolCall
shrinkAbiCall (name, vals) = do
  let numShrinkable = length $ filter canShrinkAbiValue vals

  halfwayVal <- getRandomR (0, numShrinkable)
  -- This list was made arbitrarily. Feel free to change
  let numToShrinkOptions = [1, 2, halfwayVal, numShrinkable]

  numToShrink <- min numShrinkable <$> uniform numToShrinkOptions
  shrunkVals <- shrinkVals (fromIntegral numShrinkable) (fromIntegral numToShrink) vals
  pure (name, shrunkVals)
  where
    shrinkVals 0 _ l = pure l
    shrinkVals _ 0 l = pure l
    shrinkVals _ _ [] = pure []
    shrinkVals numShrinkable numToShrink (h:t)
      | not (canShrinkAbiValue h) = (h:) <$> shrinkVals numShrinkable numToShrink t
      | otherwise = do
          -- We want to pick which ones to shrink uniformly from the vals list.
          -- Odds of shrinking one element is numToShrink/numShrinkable.
          shouldShrink <- fromList [(True, numToShrink), (False, numShrinkable-numToShrink)]
          h' <- if shouldShrink then shrinkAbiValue h else pure h
          let
            numShrinkable' = numShrinkable-1
            numToShrink' = if shouldShrink then numToShrink-1 else numToShrink
          (h':) <$> shrinkVals numShrinkable' numToShrink' t

-- | Given an 'AbiValue', generate a random \"similar\" value of the same 'AbiType'.
mutateAbiValue :: MonadRandom m => AbiValue -> m AbiValue
mutateAbiValue = \case
  AbiUInt n x -> getRandomR (0, 9 :: Int) >>= -- 10% of chance of mutation
                 \case 0 -> fixAbiUInt n <$> mutateNum x
                       _ -> pure $ AbiUInt n x
  AbiInt n x -> getRandomR (0, 9 :: Int) >>= -- 10% of chance of mutation
                \case 0 -> fixAbiInt n <$> mutateNum x
                      _ -> pure $ AbiInt n x

  AbiAddress x -> pure $ AbiAddress x -- Address are not mutated at all
  AbiBool _ -> genAbiValue AbiBoolType -- Booleans are regenerated
  AbiBytes n b -> do fs <- replicateM n getRandom
                     xs <- mutateLL (Just n) (BS.pack fs) b
                     pure $ AbiBytes n xs

  AbiBytesDynamic b -> AbiBytesDynamic <$> mutateLL Nothing mempty b
  AbiString b -> AbiString <$> mutateLL Nothing mempty b
  AbiArray n t l -> do fs <- replicateM n $ genAbiValue t
                       xs <- mutateLL (Just n) (V.fromList fs) l
                       pure $ AbiArray n t xs

  AbiArrayDynamic t l -> AbiArrayDynamic t <$> mutateLL Nothing mempty l
  AbiTuple v -> AbiTuple <$> traverse mutateAbiValue v
  AbiFunction v -> pure $ AbiFunction v

-- | Given a 'SolCall', generate a random \"similar\" call with the same 'SolSignature'.
-- Note that this funcion will mutate a *single* argument (if any)
mutateAbiCall :: MonadRandom m => SolCall -> m SolCall
mutateAbiCall = traverse f
  where f [] = pure []
        f xs = do k <- getRandomR (0, length xs - 1)
                  mv <- mutateAbiValue $ xs !! k
                  return $ replaceAt mv xs k

-- Generation, with dictionary

-- | Given a generator taking an @a@ and returning a @b@ and a way to get @b@s associated with some
-- @a@ from a 'GenDict', return a generator that takes an @a@ and either synthesizes new @b@s with the
-- provided generator or uses the 'GenDict' dictionary (when available).
genWithDict
  :: (Eq a, Ord a, MonadRandom m)
  => GenDict
  -> Map a (Set b)
  -> (a -> m b)
  -> a
  -> m b
genWithDict genDict m g t = do
  r <- getRandom
  let maybeValM = if genDict.pSynthA >= r then fromDict else pure Nothing
      fromDict = case Map.lookup t m of
                   Nothing -> pure Nothing
                   Just cs -> Just <$> rElem' cs
  fromMaybe <$> g t <*> maybeValM

-- | A small number of dummy addresses
pregenAdds :: [Addr]
pregenAdds = [i*0xffffffff | i <- [1 .. 3]]

pregenAbiAdds :: [AbiValue]
pregenAbiAdds = map (AbiAddress . fromIntegral) pregenAdds

-- | Synthesize a random 'AbiValue' given its 'AbiType'. Requires a dictionary.
-- Only produce lists with number of elements in the range [1, 32]
genAbiValueM :: MonadRandom m => GenDict -> AbiType -> m AbiValue
genAbiValueM genDict = genWithDict genDict genDict.constants $ \case
  AbiUIntType n         -> fixAbiUInt n . fromInteger <$> getRandomUint n
  AbiIntType n          -> fixAbiInt n . fromInteger <$> getRandomInt n
  AbiAddressType        -> rElem $ NE.fromList pregenAbiAdds
  AbiBoolType           -> AbiBool <$> getRandom
  AbiBytesType n        -> AbiBytes n . BS.pack . take n <$> getRandoms
  AbiBytesDynamicType   -> liftM2 (\n -> AbiBytesDynamic . BS.pack . take n)
                                  (getRandomR (1, 32)) getRandoms
  AbiStringType         -> liftM2 (\n -> AbiString       . BS.pack . take n)
                                  (getRandomR (1, 32)) getRandoms
  AbiArrayDynamicType t -> fmap (AbiArrayDynamic t) $ getRandomR (1, 32)
                           >>= flip V.replicateM (genAbiValueM genDict t)
  AbiArrayType n t      -> AbiArray n t <$> V.replicateM n (genAbiValueM genDict t)
  AbiTupleType v        -> AbiTuple <$> traverse (genAbiValueM genDict) v
  AbiFunctionType       -> liftM2 (\n -> AbiString . BS.pack . take n)
                                  (getRandomR (1, 32)) getRandoms

-- | Given a 'SolSignature', generate a random 'SolCall' with that signature,
-- possibly with a dictionary.
genAbiCallM :: MonadRandom m => GenDict -> SolSignature -> m SolCall
genAbiCallM genDict abi = do
  solCall <- genWithDict genDict
                         genDict.wholeCalls
                         (traverse $ traverse (genAbiValueM genDict))
                         abi
  mutateAbiCall solCall

-- | Given a list of 'SolSignature's, generate a random 'SolCall' for one,
-- possibly with a dictionary.
genInteractionsM
  :: MonadRandom m
  => GenDict
  -> NonEmpty SolSignature
  -> m SolCall
genInteractionsM genDict solSignatures =
  rElem solSignatures >>= genAbiCallM genDict

abiCalldata :: Text -> Vector AbiValue -> ByteString
abiCalldata s xs = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s)).unFunctionSelector
  putAbi (AbiTuple xs)
