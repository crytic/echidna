{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, TupleSections, TypeFamilies #-}

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
  , mutateCall
  , mutateCallSeq
  , mutateValue
  , prettyPrint
) where

import Control.Lens          ((<&>), (&), view)
import Control.Monad         (join, liftM2, replicateM)
import Control.Monad.Reader  (MonadReader, asks, runReaderT)
import Data.Bool             (bool)
import Data.DoubleWord       (Word128(..), Word160(..))
import Data.Monoid           ((<>))
import Data.ByteString       (ByteString)
import Data.Text             (Text, unpack)
import Data.Vector           (Vector)
import Hedgehog.Internal.Gen (MonadGen)
import GHC.Exts              (IsList(..), Item)
import Hedgehog.Range        (exponential, exponentialFrom, constant, singleton, Range)
import Numeric               (showHex)

import qualified Data.ByteString as BS
import qualified Data.List       as L
import qualified Data.Text       as T
import qualified Hedgehog.Gen    as Gen

import Echidna.Config (Config, addrList)

import EVM.ABI
import EVM.Types (Addr(..))

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
  "[" ++ L.intercalate ", " (prettyPrint <$> toList v) ++ "]"
prettyPrint (AbiArray      _ _ v) =
  "[" ++ L.intercalate ", " (prettyPrint <$> toList v) ++ "]"

encodeSig :: Text -> [AbiType] -> Text
encodeSig n ts = n <> "(" <> T.intercalate "," (map abiTypeSolidity ts) <> ")"

genSize :: MonadGen m => m Int
genSize = (8 *) <$> Gen.enum 1 32

genAbiAddress :: MonadGen m => Maybe [Addr] -> m AbiValue
genAbiAddress (Just xs) = fmap (AbiAddress . addressWord160) (Gen.element xs)
genAbiAddress _ = let w64 = Gen.word64 $ constant minBound maxBound in
  fmap AbiAddress . liftM2 Word160 Gen.enumBounded $ liftM2 Word128 w64 w64

genAbiUInt :: MonadGen m => Int -> m AbiValue
genAbiUInt n = AbiUInt n . fromInteger <$> genUInt
               where genUInt = Gen.integral $ exponential 0 $ 2^(toInteger n) - 1

genAbiInt :: MonadGen m => Int -> m AbiValue
genAbiInt n = AbiInt n . fromInteger <$> genInt
              where genInt = Gen.integral $ exponentialFrom 0 (-1 * 2 ^ (toInteger n)) (2 ^ (toInteger n - 1))

genAbiBool :: MonadGen m => m AbiValue
genAbiBool = AbiBool <$> Gen.bool

genAbiBytes :: MonadGen m => Int -> m AbiValue
genAbiBytes = liftM2 fmap AbiBytes $ Gen.bytes . singleton

genAbiBytesDynamic :: MonadGen m => m AbiValue
genAbiBytesDynamic = AbiBytesDynamic <$> Gen.bytes (constant 1 256)

genAbiString :: MonadGen m => m AbiValue
genAbiString = let fromRange = fmap AbiString . Gen.utf8 (constant 1 256) in
  Gen.choice $ fromRange <$> [Gen.ascii, Gen.digit, Gen.alpha, Gen.element ['a','b','c'], Gen.unicode]

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

genVecOfType :: (MonadReader Config m, MonadGen m) => AbiType -> Range Int -> m (Vector AbiValue)
genVecOfType t r = fmap fromList . Gen.list r $ case t of
  AbiUIntType    n    -> genAbiUInt n
  AbiIntType     n    -> genAbiInt n
  AbiAddressType      -> genAbiAddress =<< asks (view addrList)
  AbiBoolType         -> genAbiBool
  AbiBytesType   n    -> genAbiBytes n
  AbiArrayType   n t' -> genAbiArray n t'
  _ -> error "Arrays must only contain statically sized types"

genAbiArrayDynamic :: (MonadReader Config m, MonadGen m) => AbiType -> m AbiValue
genAbiArrayDynamic t = AbiArrayDynamic t <$> genVecOfType t (constant 0 256)

genAbiArray :: (MonadReader Config m, MonadGen m) => Int -> AbiType -> m AbiValue
genAbiArray n t = AbiArray n t <$> genVecOfType t (singleton n)

genAbiValue :: (MonadReader Config m, MonadGen m) => m AbiValue
genAbiValue = Gen.choice [ genAbiUInt =<< genSize
                         , genAbiInt =<< genSize
                         , genAbiAddress =<< asks (view addrList)
                         , genAbiBool
                         , genAbiBytes =<< Gen.enum 1 32
                         , genAbiBytesDynamic
                         , genAbiString
                         , genAbiArrayDynamic =<< genAbiType
                         , join $ liftM2 genAbiArray (Gen.enum 0 256) genAbiType
                         ]

genAbiValueOfType :: (MonadReader Config m, MonadGen m) => AbiType -> m AbiValue
genAbiValueOfType t = case t of
  AbiUIntType n          -> genAbiUInt n
  AbiIntType  n          -> genAbiInt n
  AbiAddressType         -> genAbiAddress =<< asks (view addrList)
  AbiBoolType            -> genAbiBool
  AbiBytesType n         -> genAbiBytes n
  AbiBytesDynamicType    -> genAbiBytesDynamic
  AbiStringType          -> genAbiString
  AbiArrayDynamicType t' -> genAbiArrayDynamic t'
  AbiArrayType n t'      -> genAbiArray n t'

genAbiCall :: (MonadReader Config m, MonadGen m) => SolSignature -> m SolCall
genAbiCall (s,ts) = (s,) <$> mapM genAbiValueOfType ts

encodeAbiCall :: SolCall -> ByteString
encodeAbiCall (t, vs) = abiCalldata t $ fromList vs

displayAbiCall :: SolCall -> String
displayAbiCall (t, vs) = unpack t ++ "(" ++ L.intercalate "," (map prettyPrint vs) ++ ")"

-- genInteractions generates a function call from a list of type signatures of
-- the form (Function name, [arg0 type, arg1 type...])
genInteractions :: MonadGen m => [SolSignature] -> Config -> m SolCall
genInteractions ls c = runReaderT (genAbiCall =<< Gen.element ls) c

type Listy t a = (IsList (t a), Item (t a) ~ a)

switchElem :: (Listy t a, MonadGen m) => m a -> t a -> m (t a)
switchElem g t = let l = toList t; n = length l in do
  i <- Gen.element [0..n]
  x <- g
  return . fromList $ take i l <> [x] <> drop (i+1) l

changeChar :: MonadGen m => ByteString -> m ByteString
changeChar = fmap BS.pack . switchElem Gen.enumBounded . BS.unpack

addBS :: MonadGen m => ByteString -> m ByteString
addBS b = Gen.element [(<> b), (b <>)] <*> Gen.utf8 (constant 0 (256 - BS.length b)) Gen.unicode

dropBS :: MonadGen m => ByteString -> m ByteString
dropBS b = Gen.choice [ BS.drop <$> Gen.element [1..BS.length b]   <*> pure b
                      , BS.take <$> Gen.element [0..BS.length b-1] <*> pure b
                      ]

changeDynamicBS :: MonadGen m => ByteString -> m ByteString
changeDynamicBS b = Gen.choice $ [changeChar, addBS, dropBS] <&> ($ b)

changeNumber :: (Enum a, Integral a, MonadGen m) => a -> m a
changeNumber n = let x = fromIntegral n :: Integer in fromIntegral . (+ x) <$> Gen.element [-10..10]

changeList :: (Listy t a, MonadGen m) => m (t a) -> m a -> t a -> m (t a)
changeList g0 g1 x = let l = toList x in
  Gen.choice [ Gen.element [(<> l), (l <>)] <*> fmap toList g0
             , drop <$> Gen.element [1..length l]   <*> pure l
             , take <$> Gen.element [0..length l-1] <*> pure l
             , switchElem g1 l
             ] <&> fromList

newOrMod ::  MonadGen m => m AbiValue -> (a -> AbiValue) -> m a -> m AbiValue
newOrMod m f n = Gen.choice [m, f <$> n]

mutateValue :: (MonadReader Config m, MonadGen m) => AbiValue -> m AbiValue
mutateValue (AbiUInt s n) =
  newOrMod (genAbiUInt s)         (AbiUInt s)         (changeNumber n)
mutateValue (AbiInt s n) =
  newOrMod (genAbiInt s)          (AbiInt s)          (changeNumber n)
mutateValue (AbiAddress a) = do
  addr <- asks (view addrList)
  newOrMod (genAbiAddress addr)   AbiAddress          (changeNumber a)
mutateValue (AbiBool _) = genAbiBool
mutateValue (AbiBytes s b) =
  newOrMod (genAbiBytes s)        (AbiBytes s)        (changeChar b)
mutateValue (AbiBytesDynamic b) =
  newOrMod genAbiBytesDynamic     AbiBytesDynamic     (changeDynamicBS b)
mutateValue (AbiString b) =
  newOrMod genAbiString           AbiString           (changeDynamicBS b)
mutateValue (AbiArrayDynamic t a) = let g0 = genVecOfType t (constant 0 (256 - length a)); g1 = genAbiValueOfType t in
  newOrMod (genAbiArrayDynamic t) (AbiArrayDynamic t) (changeList g0 g1 a)
mutateValue (AbiArray s t a) =
  newOrMod (genAbiArray s t)      (AbiArray s t)      (switchElem (genAbiValueOfType t) a)

changeOrId :: (Traversable t, MonadGen m) => (a -> m a) -> t a -> m (t a)
changeOrId f = mapM $ (Gen.element [f, pure] >>=) . (&)

mutateCall :: MonadGen m => Config -> SolCall -> m SolCall
mutateCall c (t, vs) = runReaderT ((t,) <$> changeOrId mutateValue vs) c

mutateCallSeq :: MonadGen m => [SolSignature] -> [SolCall] -> Config -> m [SolCall]
mutateCallSeq s cs c = let g = genInteractions s c in
  changeOrId (mutateCall c) cs >>= changeList (Gen.element [1..10] >>= flip replicateM g) g
