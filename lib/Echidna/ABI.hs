{-# LANGUAGE TupleSections #-}

module Echidna.ABI (
    SolCall
  , SolSignature
  , encodeAbiCall
  , encodeSig
  , displayAbiCall
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
  , prettyPrint
) where

import Control.Monad         (join, liftM2)
import Data.Bool             (bool)
import Data.DoubleWord       (Word128(..), Word160(..))
import Data.Monoid           ((<>))
import Data.ByteString       (ByteString)
import Data.Text             (Text, unpack)
import Data.Vector           (Vector, fromList, toList)
import Hedgehog.Internal.Gen (MonadGen)
import Hedgehog.Range        (exponential, constant, singleton, Range)
import Numeric               (showHex)

import qualified Data.List    as L
import qualified Data.Text    as T
import qualified Hedgehog.Gen as Gen

import EVM.ABI
import EVM.Types ()

type SolCall = (Text, [AbiValue])

type SolSignature = (Text, [AbiType])

prettyPrint :: AbiValue -> String
prettyPrint (AbiUInt _ n)         = show n
prettyPrint (AbiInt  _ n)         = show n
prettyPrint (AbiAddress n)        = showHex n ""
prettyPrint (AbiBool b)           = bool "true" "false" b
prettyPrint (AbiBytes      _ b)   = show b
prettyPrint (AbiBytesDynamic b)   = show b
prettyPrint (AbiString       s)   = show s
prettyPrint (AbiArrayDynamic _ v) =
  "[" ++ L.intercalate ", " (map prettyPrint $ toList v) ++ "]"
prettyPrint (AbiArray      _ _ v) =
  "[" ++ L.intercalate ", " (map prettyPrint $ toList v) ++ "]"

encodeSig :: Text -> [AbiType] -> Text
encodeSig n ts = n <> "(" <> T.intercalate "," (map abiTypeSolidity ts) <> ")"

genSize :: MonadGen m => m Int
genSize = (8 *) <$> Gen.enum 1 32

genAbiAddress :: MonadGen m => m AbiValue
genAbiAddress = let w64 = Gen.word64 $ constant minBound maxBound in
  fmap AbiAddress . liftM2 Word160 Gen.enumBounded $ liftM2 Word128 w64 w64

genAbiUInt :: MonadGen m => Int -> m AbiValue
genAbiUInt n = AbiUInt n . fromInteger <$> genUInt
               where genUInt = Gen.integral $ exponential 0 $ 2^(toInteger n) - 1

genAbiInt :: MonadGen m => Int -> m AbiValue
genAbiInt n = AbiInt n . fromInteger <$> genInt
              where genInt = liftM2 (*)
                              (Gen.integral $ exponential 0 $ 2^(toInteger n - 1) - 1)
                              (Gen.element [1, -1])

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
  AbiUIntType    n    -> genAbiUInt n
  AbiIntType     n    -> genAbiInt n
  AbiAddressType      -> genAbiAddress
  AbiBoolType         -> genAbiBool
  AbiBytesType   n    -> genAbiBytes n
  AbiArrayType   n t' -> genAbiArray n t'
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

genAbiCall :: MonadGen m => SolSignature -> m SolCall
genAbiCall (s,ts) = (s,) <$> mapM genAbiValueOfType ts

encodeAbiCall :: SolCall -> ByteString
encodeAbiCall (t, vs) = abiCalldata t $ fromList vs

displayAbiCall :: SolCall -> String
displayAbiCall (t, vs) = unpack t ++ "(" ++ L.intercalate "," (map prettyPrint vs) ++ ")"

-- genInteractions generates a function call from a list of type signatures of
-- the form (Function name, [arg0 type, arg1 type...])
genInteractions :: MonadGen m => [SolSignature] -> m SolCall
genInteractions ls = genAbiCall =<< Gen.element ls
