module Echidna.ABI (
    encodeSig
  , genAbiAddress
  , genAbiArray
  , genAbiArrayDynamic
  , genAbiBool
  , genAbiBytes
  , genAbiBytesDynamic
  , genAbiInt
  , genInteractions
  , genAbiString
  , genAbiType
  , genAbiUInt
  , genAbiValue
) where

import Control.Arrow         (second)
import Control.Monad         (join, liftM2)
import Data.DoubleWord       (Word128(..), Word160(..), Word256(..), signedWord)
import Data.ByteString       (ByteString)
import Data.Monoid           ((<>))
import Data.Text             (Text, intercalate)
import Data.Vector           (Vector, fromList, toList)
import Hedgehog.Internal.Gen (MonadGen)
import Hedgehog.Range        (constant, singleton, Range)

import qualified Hedgehog.Gen as Gen

import EVM.ABI
import EVM.Types ()

encodeSig :: Text -> Vector AbiType -> Text
encodeSig n ts = n <> "(" <> intercalate "," (map abiTypeSolidity $ toList ts) <> ")"

genSize :: MonadGen m => m Int
genSize = (8 *) <$> Gen.enum 1 32

genWord256 :: MonadGen m => m Word256
genWord256 = let w64 = Gen.word64 $ constant minBound maxBound in
  liftM2 Word256 (liftM2 Word128 w64 w64) (liftM2 Word128 w64 w64)

genAbiAddress :: MonadGen m => m AbiValue
genAbiAddress = let w64 = Gen.word64 $ constant minBound maxBound in
  fmap AbiAddress . liftM2 Word160 Gen.enumBounded $ liftM2 Word128 w64 w64

genAbiUInt :: MonadGen m => Int -> m AbiValue
genAbiUInt n = AbiUInt n . scale <$> genWord256 where
  scale = if n == 256 then id else flip mod (2^n)

genAbiInt :: MonadGen m => Int -> m AbiValue
genAbiInt n = AbiInt n . signedWord . scale <$> genWord256 where
  scale = if n == 256 then id else flip mod (2^n)

genAbiBool :: MonadGen m => m AbiValue
genAbiBool = AbiBool <$> Gen.bool

genAbiBytes :: MonadGen m => Int -> m AbiValue
genAbiBytes = liftM2 fmap AbiBytes $ Gen.bytes . singleton

genAbiBytesDynamic :: MonadGen m => m AbiValue
genAbiBytesDynamic = AbiBytesDynamic <$> Gen.bytes (constant 1 256)

genAbiString :: MonadGen m => m AbiValue
genAbiString = AbiString <$> Gen.utf8 (constant 1 256) Gen.unicode

genStaticAbiType :: MonadGen m => m AbiType
genStaticAbiType = go (16 :: Int) where
  go n = Gen.choice $ [ AbiUIntType <$> genSize
                      , AbiIntType <$> genSize
                      , pure AbiAddressType
                      , pure AbiBoolType
                      , AbiBytesType <$> Gen.enum 1 32
                      ] ++ [AbiArrayType <$> Gen.enum 0 256 <*> go (n - 1) | n > 0] 

genAbiType :: MonadGen m => m AbiType
genAbiType = Gen.choice [ pure AbiBytesDynamicType
                        , pure AbiStringType
                        , AbiArrayDynamicType <$> genStaticAbiType
                        , genStaticAbiType
                        ]

genVecOfType :: MonadGen m => AbiType -> Range Int -> m (Vector AbiValue)
genVecOfType t r = fmap fromList . Gen.list r $ case t of
  AbiUIntType         n    -> genAbiUInt n
  AbiIntType          n    -> genAbiInt n
  AbiAddressType           -> genAbiAddress
  AbiBoolType              -> genAbiBool
  AbiBytesType        n    -> genAbiBytes n
  AbiArrayType        n t' -> genAbiArray n t'
  _ -> error "Arrays must only contain statically sized types"

genAbiArrayDynamic :: MonadGen m => AbiType -> m AbiValue
genAbiArrayDynamic t = AbiArrayDynamic t <$> genVecOfType t (constant 0 256)

genAbiArray :: MonadGen m => Int -> AbiType -> m AbiValue
genAbiArray n t = AbiArray n t <$> genVecOfType t (singleton n)

genAbiValue :: MonadGen m => m AbiValue
genAbiValue = Gen.choice [ genAbiUInt =<< genSize
                         , genAbiInt =<< genSize
                         , genAbiAddress
                         , genAbiBool
                         , genAbiBytes =<< Gen.enum 1 32
                         , genAbiBytesDynamic
                         , genAbiString
                         , genAbiArrayDynamic =<< genAbiType
                         , join $ liftM2 genAbiArray (Gen.enum 0 256) genAbiType
                         ]

genAbiValueOfType :: MonadGen m => AbiType -> m AbiValue
genAbiValueOfType t = case t of
  AbiUIntType n          -> genAbiUInt n
  AbiIntType  n          -> genAbiInt n
  AbiAddressType         -> genAbiAddress
  AbiBoolType            -> genAbiBool
  AbiBytesType n         -> genAbiBytes n
  AbiBytesDynamicType    -> genAbiBytesDynamic
  AbiStringType          -> genAbiString
  AbiArrayDynamicType t' -> genAbiArrayDynamic t'
  AbiArrayType n t'      -> genAbiArray n t'

genAbiCall :: MonadGen m => Text -> Vector AbiType -> m ByteString
genAbiCall s ts = abiCalldata (encodeSig s ts) <$> mapM genAbiValueOfType ts

-- genInteractions generates a function call from a list of type signatures of
-- the form (Function name, [arg0 type, arg1 type...])
genInteractions :: MonadGen m => [(Text, [AbiType])] -> m ByteString
genInteractions ls = uncurry genAbiCall . second fromList =<< Gen.element ls
