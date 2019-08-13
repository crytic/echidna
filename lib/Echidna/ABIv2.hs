{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- The entirety of this file is pretty much stolen from EVM.Solidity and EVM.ABI
-- and modified to work with ABIv2 by adding a Tuple entry to AbiType and
-- AbiValue.

module Echidna.ABIv2 where

import Control.Lens.TH            (makeLenses)
import Control.Lens hiding        (Indexed)
import Control.Monad              (void, forM_, replicateM_, replicateM)
import Data.Aeson                 (Value(..))
import Data.Aeson.Lens
import Data.Binary.Get            (Get, label, getWord8, getWord32be, skip)
import Data.Binary.Put            (Put, runPut, putWord8, putWord32be)
import Data.Bits                  (shiftL, shiftR, (.&.))
import Data.DoubleWord            (Word256, Int256, Word160)
import Data.Foldable              (toList, fold)
import Data.Functor               (($>))
import Data.Hashable              (Hashable(..))
import Data.Maybe                 (fromMaybe, fromJust)
import Data.Sequence              (Seq)
import Data.Text                  (Text)
import Data.Text.Encoding         (encodeUtf8)
import Data.Word                  (Word8, Word32)
import GHC.Generics               (Generic)

import EVM.ABI      (Indexed(..), Anonymity(..))
import EVM.Keccak   (keccak, abiKeccak)
import EVM.Solidity (SrcMap, SourceCache(..), makeSrcMaps, stripBytecodeMetadata)
import EVM.Types    (W256)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSLazy
import qualified Data.ByteString.Base16 as BS16
import qualified Data.HashMap.Strict    as M
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Vector            as V
import qualified Text.Megaparsec        as P
import qualified Text.Megaparsec.Char   as P

-- types for EVM ABIv2
data AbiValue2 = AbiUInt2         Int Word256
               | AbiInt2          Int Int256
               | AbiAddress2      Word160
               | AbiBool2         Bool
               | AbiBytes2        Int BS.ByteString
               | AbiBytesDynamic2 BS.ByteString
               | AbiString2       BS.ByteString
               | AbiArrayDynamic2 AbiType2 (V.Vector AbiValue2)
               | AbiArray2        Int AbiType2 (V.Vector AbiValue2)
               | AbiTuple2        (V.Vector AbiValue2)
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
              | AbiTupleType2        (V.Vector AbiType2)
  deriving (Show, Read, Eq, Ord, Generic)

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
  AbiTupleType2 v        -> "(" <> (T.intercalate "," . V.toList $ abiTypeSolidity2 <$> v) <> ")"

data AbiKind = Dynamic | Static
  deriving (Show, Read, Eq, Ord)

abiKind2 :: AbiType2 -> AbiKind
abiKind2 = \case
  AbiBytesDynamicType2   -> Dynamic
  AbiStringType2         -> Dynamic
  AbiArrayDynamicType2 _ -> Dynamic
  AbiArrayType2 _ t      -> abiKind2 t
  AbiTupleType2 v        -> if Dynamic `elem` (abiKind2 <$> v) then Dynamic else Static
  _                      -> Static

-- orphan instance for Hashable a => Hashable (Vector a)
instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s = hashWithSalt s . V.toList

data SolcContract2 = SolcContract2
  { _runtimeCodehash2  :: W256
  , _creationCodehash2 :: W256
  , _runtimeCode2      :: BS.ByteString
  , _creationCode2     :: BS.ByteString
  , _contractName2     :: Text
  , _constructorInputs2 :: [(Text, AbiType2)]
  , _abiMap2           :: Map.Map Word32 Method2
  , _eventMap2         :: Map.Map W256 Event2
  , _runtimeSrcmap2    :: Seq SrcMap
  , _creationSrcmap2   :: Seq SrcMap
  , _contractAst2      :: Value
  } deriving (Show, Eq)

data Method2 = Method2
  { _methodOutput2 :: Maybe (Text, AbiType2)
  , _methodInputs2 :: [(Text, AbiType2)]
  , _methodName2 :: Text
  , _methodSignature2 :: Text
  } deriving (Show, Eq, Ord)

data Event2 = Event2 Text Anonymity [(AbiType2, Indexed)]
  deriving (Show, Ord, Eq)

makeLenses ''SolcContract2
makeLenses ''Method2

toCode :: Text -> BS.ByteString
toCode = fst . BS16.decode . encodeUtf8

force :: String -> Maybe a -> a
force s = fromMaybe (error s)

parseMethodInput2 :: (Show s, AsValue s) => s -> (Text, AbiType2)
parseMethodInput2 x =
  ( x ^?! key "name" . _String
  , force "internal error: method type" (parseTypeName2 (x ^? key "components" . _Array) (x ^?! key "type" . _String))
  )

readSolc2 :: FilePath -> IO (Maybe (Map.Map Text SolcContract2, SourceCache))
readSolc2 fp =
  (readJSON2 <$> TIO.readFile fp) >>=
    \case
      Nothing -> return Nothing
      Just (cs, asts, sources) -> do
        sc <- makeSourceCache sources asts
        return $! Just (cs, sc)

readJSON2 :: Text -> Maybe (Map.Map Text SolcContract2, Map.Map Text Value, [Text])
readJSON2 json = do
  cs <-
    f <$> (json ^? key "contracts" . _Object)
      <*> (fmap (fmap (^. _String)) $ json ^? key "sourceList" . _Array)
  sources <- toList . fmap (view _String) <$> json ^? key "sourceList" . _Array
  return (cs, Map.fromList (M.toList asts), sources)
  where
    asts = fromMaybe (error "JSON lacks abstract syntax trees.") (json ^? key "sources" . _Object)
    f x y = Map.fromList . map (g y) . M.toList $ x
    g _ (s, x) =
      let
        theRuntimeCode = toCode (x ^?! key "bin-runtime" . _String)
        theCreationCode = toCode (x ^?! key "bin" . _String)
        abis =
          toList ((x ^?! key "abi" . _String) ^?! _Array)
      in (s, SolcContract2 {
        _runtimeCode2      = theRuntimeCode,
        _creationCode2     = theCreationCode,
        _runtimeCodehash2  = keccak (stripBytecodeMetadata theRuntimeCode),
        _creationCodehash2 = keccak (stripBytecodeMetadata theCreationCode),
        _runtimeSrcmap2    = force "internal error: srcmap-runtime" (makeSrcMaps (x ^?! key "srcmap-runtime" . _String)),
        _creationSrcmap2   = force "internal error: srcmap" (makeSrcMaps (x ^?! key "srcmap" . _String)),
        _contractName2 = s,
        _contractAst2 =
          fromMaybe
            (error "JSON lacks abstract syntax trees.")
            (preview (ix (T.split (== ':') s !! 0) . key "AST") asts),

        _constructorInputs2 =
          let
            isConstructor y =
              "constructor" == y ^?! key "type" . _String
          in
            case filter isConstructor abis of
              [abi] -> map parseMethodInput2 (toList (abi ^?! key "inputs" . _Array))
              [] -> [] -- default constructor has zero inputs
              _  -> error "strange: contract has multiple constructors",

        _abiMap2       = Map.fromList $
          let
            relevant =
              filter (\y -> "function" == y ^?! key "type" . _String) abis
          in flip map relevant $
            \abi -> (
              abiKeccak (encodeUtf8 (signature abi)),
              Method2
                { _methodName2 = abi ^?! key "name" . _String
                , _methodSignature2 = signature abi
                , _methodInputs2 =
                    map parseMethodInput2
                      (toList (abi ^?! key "inputs" . _Array))
                , _methodOutput2 =
                    fmap parseMethodInput2
                      (abi ^? key "outputs" . _Array . ix 0)
                }
            ),
        _eventMap2     = Map.fromList $
          flip map (filter (\y -> "event" == y ^?! key "type" . _String)
                     . toList $ (x ^?! key "abi" . _String) ^?! _Array) $
            \abi ->
              ( keccak (encodeUtf8 (signature abi))
              , Event2
                  (abi ^?! key "name" . _String)
                  (if abi ^?! key "anonymous" . _Bool then Anonymous else NotAnonymous)
                  (map (\y -> ( force "internal error: type" (parseTypeName2 (y ^? key "components" . _Array) (y ^?! key "type" . _String))
                              , if y ^?! key "indexed" . _Bool
                                then Indexed
                                else NotIndexed ))
                    (toList $ abi ^?! key "inputs" . _Array))
              )
      })

signature :: AsValue s => s -> Text
signature abi =
  case abi ^?! key "type" of
    "fallback" -> "<fallback>"
    _ ->
      fold [
        fromMaybe "<constructor>" (abi ^? key "name" . _String), "(",
        T.intercalate ","
          (map (\x -> x ^?! key "type" . _String)
            (V.toList $ abi ^?! key "inputs" . _Array)),
        ")"
      ]

makeSourceCache :: [Text] -> Map.Map Text Value -> IO SourceCache
makeSourceCache paths asts = do
  xs <- mapM (BS.readFile . T.unpack) paths
  return $! SourceCache
    { _snippetCache = mempty
    , _sourceFiles =
        Map.fromList (zip [0..] (zip paths xs))
    , _sourceLines =
        Map.fromList (zip [0 .. length paths - 1]
                       (map (V.fromList . BS.split 0xa) xs))
    , _sourceAsts =
        asts
    }

parseTypeName2 :: Maybe (V.Vector Value) -> Text -> Maybe AbiType2
parseTypeName2 v = P.parseMaybe (typeWithArraySuffix (fromMaybe (V.fromList []) v))

typeWithArraySuffix :: V.Vector Value -> P.Parsec () Text AbiType2
typeWithArraySuffix v = do
  base <- basicType2 v
  sizes <-
    P.many $
      P.between
        (P.char '[') (P.char ']')
        (P.many P.digitChar)

  let
    parseSize :: AbiType2 -> String -> AbiType2
    parseSize t "" = AbiArrayDynamicType2 t
    parseSize t s  = AbiArrayType2 (read s) t

  pure (foldl parseSize base sizes)

basicType2 :: V.Vector Value -> P.Parsec () Text AbiType2
basicType2 v =
  P.choice
    [ P.string "address" $> AbiAddressType2
    , P.string "bool"    $> AbiBoolType2
    , P.string "string"  $> AbiStringType2

    , sizedType "uint" AbiUIntType2
    , sizedType "int"  AbiIntType2
    , sizedType "bytes" AbiBytesType2

    , P.string "bytes" $> AbiBytesDynamicType2
    , P.string "tuple" $> (AbiTupleType2 tupleTypes)
    ]

  where
    sizedType :: Text -> (Int -> AbiType2) -> P.Parsec () Text AbiType2
    sizedType s f = P.try $ do
      void (P.string s)
      fmap (f . read) (P.some P.digitChar)
    tupleTypes = catMaybes' $ parseTypeName2' <$> v
    parseTypeName2' x = parseTypeName2 (x ^? key "components" . _Array) (x ^?! key "type" . _String)
    catMaybes' = fmap fromJust . V.filter (/= Nothing)


-- ABI encode/decode functions
encodeAbiValue2 :: AbiValue2 -> BS.ByteString
encodeAbiValue2 = BSLazy.toStrict . runPut . putAbi2

abiCalldata2 :: T.Text -> V.Vector AbiValue2 -> BS.ByteString
abiCalldata2 s xs = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s))
  putAbiSeq2 xs

getAbi2 :: AbiType2 -> Get AbiValue2
getAbi2 t = label (T.unpack (abiTypeSolidity2 t)) $
  case t of
    AbiUIntType2 n  -> do
      let word32Count = 8 * div (n + 255) 256
      xs <- replicateM word32Count getWord32be
      pure (AbiUInt2 n (pack32 word32Count xs))

    AbiIntType2 n   -> asUInt n (AbiInt2 n)
    AbiAddressType2 -> asUInt 256 AbiAddress2
    AbiBoolType2    -> asUInt 256 (AbiBool2 . (== (1 :: Int)))

    AbiBytesType2 n ->
      AbiBytes2 n <$> getBytesWith256BitPadding n

    AbiBytesDynamicType2 ->
      AbiBytesDynamic2 <$>
        (label "bytes length prefix" getWord256
          >>= label "bytes data" . getBytesWith256BitPadding)

    AbiStringType2 -> do
      AbiBytesDynamic2 x <- getAbi2 AbiBytesDynamicType2
      pure (AbiString2 x)

    AbiArrayType2 n t' ->
      AbiArray2 n t' <$> getAbiSeq2 n (repeat t')

    AbiArrayDynamicType2 t' -> do
      AbiUInt2 _ n <- label "array length" (getAbi2 (AbiUIntType2 256))
      AbiArrayDynamic2 t' <$>
        label "array body" (getAbiSeq2 (fromIntegral n) (repeat t'))

    AbiTupleType2 v ->
      AbiTuple2 <$> getAbiSeq2 (V.length v) (V.toList v)

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

getAbiSeq2 :: Int -> [AbiType2] -> Get (V.Vector AbiValue2)
getAbiSeq2 n ts = label "sequence" $ do
  hs <- label "sequence head" (getAbiHead2 n ts)
  V.fromList <$>
    label "sequence tail" (mapM (either getAbi2 pure) hs)

getAbiHead2 :: Int -> [AbiType2]
  -> Get [Either AbiType2 AbiValue2]
getAbiHead2 0 _      = pure []
getAbiHead2 _ []     = fail "ran out of types"
getAbiHead2 n (t:ts) = do
  case abiKind2 t of
    Dynamic ->
      (Left t :) <$> (skip 32 *> getAbiHead2 (n - 1) ts)
    Static ->
      do x  <- getAbi2 t
         xs <- getAbiHead2 (n - 1) ts
         pure (Right x : xs)

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

getBytesWith256BitPadding :: Integral a => a -> Get BS.ByteString
getBytesWith256BitPadding i =
  (BS.pack <$> replicateM n getWord8)
    <* skip ((roundTo256Bits n) - n)
  where n = fromIntegral i

pack32 :: Int -> [Word32] -> Word256
pack32 n xs =
  sum [ shiftL x ((n - i) * 32)
      | (x, i) <- zip (map fromIntegral xs) [1..] ]

pack8 :: Int -> [Word8] -> Word256
pack8 n xs =
  sum [ shiftL x ((n - i) * 8)
      | (x, i) <- zip (map fromIntegral xs) [1..] ]

asUInt :: Integral i => Int -> (i -> a) -> Get a
asUInt n f = (\(AbiUInt2 _ x) -> f (fromIntegral x)) <$> getAbi2 (AbiUIntType2 n)

getWord256 :: Get Word256
getWord256 = pack32 8 <$> replicateM 8 getWord32be
