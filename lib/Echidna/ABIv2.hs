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
data AbiValue = AbiUInt         Int Word256
              | AbiInt          Int Int256
              | AbiAddress      Word160
              | AbiBool         Bool
              | AbiBytes        Int BS.ByteString
              | AbiBytesDynamic BS.ByteString
              | AbiString       BS.ByteString
              | AbiArrayDynamic AbiType (V.Vector AbiValue)
              | AbiArray        Int AbiType (V.Vector AbiValue)
              | AbiTuple        (V.Vector AbiValue)
  deriving (Show, Read, Eq, Ord, Generic)

data AbiType = AbiUIntType         Int
             | AbiIntType          Int
             | AbiAddressType
             | AbiBoolType
             | AbiBytesType        Int
             | AbiBytesDynamicType
             | AbiStringType
             | AbiArrayDynamicType AbiType
             | AbiArrayType        Int AbiType
             | AbiTupleType        (V.Vector AbiType)
  deriving (Show, Read, Eq, Ord, Generic)

abiValueType :: AbiValue -> AbiType
abiValueType = \case
  AbiUInt n _         -> AbiUIntType n
  AbiInt n _          -> AbiIntType  n
  AbiAddress _        -> AbiAddressType
  AbiBool _           -> AbiBoolType
  AbiBytes n _        -> AbiBytesType n
  AbiBytesDynamic _   -> AbiBytesDynamicType
  AbiString _         -> AbiStringType
  AbiArrayDynamic t _ -> AbiArrayDynamicType t
  AbiArray n t _      -> AbiArrayType n t
  AbiTuple v          -> AbiTupleType (abiValueType <$> v)

abiTypeSolidity :: AbiType -> Text
abiTypeSolidity = \case
  AbiUIntType n         -> "uint" <> T.pack (show n)
  AbiIntType n          -> "int" <> T.pack (show n)
  AbiAddressType        -> "address"
  AbiBoolType           -> "bool"
  AbiBytesType n        -> "bytes" <> T.pack (show n)
  AbiBytesDynamicType   -> "bytes"
  AbiStringType         -> "string"
  AbiArrayDynamicType t -> abiTypeSolidity t <> "[]"
  AbiArrayType n t      -> abiTypeSolidity t <> "[" <> T.pack (show n) <> "]"
  AbiTupleType v        -> "(" <> (T.intercalate "," . V.toList $ abiTypeSolidity <$> v) <> ")"
  --AbiTupleType _        -> "tuple"

data AbiKind = Dynamic | Static
  deriving (Show, Read, Eq, Ord)

abiKind :: AbiType -> AbiKind
abiKind = \case
  AbiBytesDynamicType   -> Dynamic
  AbiStringType         -> Dynamic
  AbiArrayDynamicType _ -> Dynamic
  AbiArrayType _ t      -> abiKind t
  AbiTupleType v        -> if Dynamic `elem` (abiKind <$> v) then Dynamic else Static
  _                      -> Static

-- orphan instance for Hashable a => Hashable (Vector a)
instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt s = hashWithSalt s . V.toList

data SolcContract = SolcContract
  { _runtimeCodehash  :: W256
  , _creationCodehash :: W256
  , _runtimeCode      :: BS.ByteString
  , _creationCode     :: BS.ByteString
  , _contractName     :: Text
  , _constructorInputs :: [(Text, AbiType)]
  , _abiMap           :: Map.Map Word32 Method
  , _eventMap         :: Map.Map W256 Event
  , _runtimeSrcmap    :: Seq SrcMap
  , _creationSrcmap   :: Seq SrcMap
  , _contractAst      :: Value
  } deriving (Show, Eq)

data Method = Method
  { _methodOutput :: Maybe (Text, AbiType)
  , _methodInputs :: [(Text, AbiType)]
  , _methodName :: Text
  , _methodSignature :: Text
  } deriving (Show, Eq, Ord)

data Event = Event Text Anonymity [(AbiType, Indexed)]
  deriving (Show, Ord, Eq)

makeLenses ''SolcContract
makeLenses ''Method

toCode :: Text -> BS.ByteString
toCode = fst . BS16.decode . encodeUtf8

force :: String -> Maybe a -> a
force s = fromMaybe (error s)

parseMethodInput :: (Show s, AsValue s) => s -> (Text, AbiType)
parseMethodInput x =
  ( x ^?! key "name" . _String
  , force "internal error: method type" (parseTypeName (x ^? key "components" . _Array) (x ^?! key "type" . _String))
  )

readSolc :: FilePath -> IO (Maybe (Map.Map Text SolcContract, SourceCache))
readSolc fp =
  (readJSON <$> TIO.readFile fp) >>=
    \case
      Nothing -> return Nothing
      Just (cs, asts, sources) -> do
        sc <- makeSourceCache sources asts
        return (Just (cs, sc))

readJSON :: Text -> Maybe (Map.Map Text SolcContract, Map.Map Text Value, [Text])
readJSON json = do
  cs <-
    f <$> (json ^? key "contracts" . _Object)
      <*> fmap (fmap (^. _String)) (json ^? key "sourceList" . _Array)
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
      in (s, SolcContract {
        _runtimeCode      = theRuntimeCode,
        _creationCode     = theCreationCode,
        _runtimeCodehash  = keccak (stripBytecodeMetadata theRuntimeCode),
        _creationCodehash = keccak (stripBytecodeMetadata theCreationCode),
        _runtimeSrcmap    = force "internal error: srcmap-runtime" (makeSrcMaps (x ^?! key "srcmap-runtime" . _String)),
        _creationSrcmap   = force "internal error: srcmap" (makeSrcMaps (x ^?! key "srcmap" . _String)),
        _contractName = s,
        _contractAst =
          fromMaybe
            (error "JSON lacks abstract syntax trees.")
            (preview (ix (head $ T.split (== ':') s) . key "AST") asts),

        _constructorInputs =
          let
            isConstructor y =
              "constructor" == y ^?! key "type" . _String
          in
            case filter isConstructor abis of
              [abi] -> map parseMethodInput (toList (abi ^?! key "inputs" . _Array))
              [] -> [] -- default constructor has zero inputs
              _  -> error "strange: contract has multiple constructors",

        _abiMap       = Map.fromList $
          let
            relevant =
              filter (\y -> "function" == y ^?! key "type" . _String) abis
          in flip map relevant $
            \abi -> (
              abiKeccak (encodeUtf8 (signature abi)),
              Method
                { _methodName = abi ^?! key "name" . _String
                , _methodSignature = signature abi
                , _methodInputs =
                    map parseMethodInput
                      (toList (abi ^?! key "inputs" . _Array))
                , _methodOutput =
                    fmap parseMethodInput
                      (abi ^? key "outputs" . _Array . ix 0)
                }
            ),
        _eventMap     = Map.fromList $
          flip map (filter (\y -> "event" == y ^?! key "type" . _String)
                     . toList $ (x ^?! key "abi" . _String) ^?! _Array) $
            \abi ->
              ( keccak (encodeUtf8 (signature abi))
              , Event
                  (abi ^?! key "name" . _String)
                  (if abi ^?! key "anonymous" . _Bool then Anonymous else NotAnonymous)
                  (map (\y -> ( force "internal error: type" (parseTypeName (y ^? key "components" . _Array) (y ^?! key "type" . _String))
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

parseTypeName :: Maybe (V.Vector Value) -> Text -> Maybe AbiType
parseTypeName v = P.parseMaybe (typeWithArraySuffix (fromMaybe (V.fromList []) v))

typeWithArraySuffix :: V.Vector Value -> P.Parsec () Text AbiType
typeWithArraySuffix v = do
  base <- basicType v
  sizes <-
    P.many $
      P.between
        (P.char '[') (P.char ']')
        (P.many P.digitChar)

  let
    parseSize :: AbiType -> String -> AbiType
    parseSize t "" = AbiArrayDynamicType t
    parseSize t s  = AbiArrayType (read s) t

  pure (foldl parseSize base sizes)

basicType :: V.Vector Value -> P.Parsec () Text AbiType
basicType v =
  P.choice
    [ P.string "address" $> AbiAddressType
    , P.string "bool"    $> AbiBoolType
    , P.string "string"  $> AbiStringType

    , sizedType "uint" AbiUIntType
    , sizedType "int"  AbiIntType
    , sizedType "bytes" AbiBytesType

    , P.string "bytes" $> AbiBytesDynamicType
    , P.string "tuple" $> AbiTupleType tupleTypes
    ]

  where
    sizedType :: Text -> (Int -> AbiType) -> P.Parsec () Text AbiType
    sizedType s f = P.try $ do
      void (P.string s)
      fmap (f . read) (P.some P.digitChar)
    tupleTypes = catMaybes' $ parseTypeName' <$> v
    parseTypeName' x = parseTypeName (x ^? key "components" . _Array) (x ^?! key "type" . _String)
    catMaybes' = fmap fromJust . V.filter (/= Nothing)


-- ABI encode/decode functions
encodeAbiValue :: AbiValue -> BS.ByteString
encodeAbiValue = BSLazy.toStrict . runPut . putAbi

abiCalldata :: T.Text -> V.Vector AbiValue -> BS.ByteString
abiCalldata s xs = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s))
  putAbiSeq xs

getAbi :: AbiType -> Get AbiValue
getAbi t = label (T.unpack (abiTypeSolidity t)) $
  case t of
    AbiUIntType n  -> do
      let word32Count = 8 * div (n + 255) 256
      xs <- replicateM word32Count getWord32be
      pure (AbiUInt n (pack32 word32Count xs))

    AbiIntType n   -> asUInt n (AbiInt n)
    AbiAddressType -> asUInt 256 AbiAddress
    AbiBoolType    -> asUInt 256 (AbiBool . (== (1 :: Int)))

    AbiBytesType n ->
      AbiBytes n <$> getBytesWith256BitPadding n

    AbiBytesDynamicType ->
      AbiBytesDynamic <$>
        (label "bytes length prefix" getWord256
          >>= label "bytes data" . getBytesWith256BitPadding)

    AbiStringType -> do
      AbiBytesDynamic x <- getAbi AbiBytesDynamicType
      pure (AbiString x)

    AbiArrayType n t' ->
      AbiArray n t' <$> getAbiSeq n (repeat t')

    AbiArrayDynamicType t' -> do
      AbiUInt _ n <- label "array length" (getAbi (AbiUIntType 256))
      AbiArrayDynamic t' <$>
        label "array body" (getAbiSeq (fromIntegral n) (repeat t'))

    AbiTupleType v ->
      AbiTuple <$> getAbiSeq (V.length v) (V.toList v)

putAbi :: AbiValue -> Put
putAbi = \case
  AbiUInt n x -> do
    let word32Count = div (roundTo256Bits n) 4
    forM_ (reverse [0 .. word32Count - 1]) $ \i ->
      putWord32be (fromIntegral (shiftR x (i * 32) .&. 0xffffffff))

  AbiInt n x   -> putAbi (AbiUInt n (fromIntegral x))
  AbiAddress x -> putAbi (AbiUInt 160 (fromIntegral x))
  AbiBool x    -> putAbi (AbiUInt 8 (if x then 1 else 0))

  AbiBytes n xs -> do
    forM_ [0 .. n-1] (putWord8 . BS.index xs)
    replicateM_ (roundTo256Bits n - n) (putWord8 0)

  AbiBytesDynamic xs -> do
    let n = BS.length xs
    putAbi (AbiUInt 256 (fromIntegral n))
    putAbi (AbiBytes n xs)

  AbiString s ->
    putAbi (AbiBytesDynamic s)

  AbiArray _ _ xs ->
    putAbiSeq xs

  AbiArrayDynamic _ xs -> do
    putAbi (AbiUInt 256 (fromIntegral (V.length xs)))
    putAbiSeq xs

  AbiTuple v ->
    putAbiSeq v

putAbiSeq :: V.Vector AbiValue -> Put
putAbiSeq xs =
  do snd $ V.foldl' f (headSize, pure ()) (V.zip xs tailSizes)
     V.sequence_ (V.map putAbiTail xs)
  where
    headSize = V.sum $ V.map abiHeadSize xs
    tailSizes = V.map abiTailSize xs
    -- f is like a putHead
    f (i, m) (x, j) =
      case abiKind (abiValueType x) of
        Static -> (i, m >> putAbi x)
        Dynamic -> (i + j, m >> putAbi (AbiUInt 256 (fromIntegral i)))

getAbiSeq :: Int -> [AbiType] -> Get (V.Vector AbiValue)
getAbiSeq n ts = label "sequence" $ do
  hs <- label "sequence head" (getAbiHead n ts)
  V.fromList <$>
    label "sequence tail" (mapM (either getAbi pure) hs)

getAbiHead :: Int -> [AbiType]
  -> Get [Either AbiType AbiValue]
getAbiHead 0 _      = pure []
getAbiHead _ []     = fail "ran out of types"
getAbiHead n (t:ts) =
  case abiKind t of
    Dynamic ->
      (Left t :) <$> (skip 32 *> getAbiHead (n - 1) ts)
    Static ->
      do x  <- getAbi t
         xs <- getAbiHead (n - 1) ts
         pure (Right x : xs)

putAbiTail :: AbiValue -> Put
putAbiTail x =
  case abiKind (abiValueType x) of
    -- static types always have tail = ""
    Static  -> pure ()
    -- dynamic types (even in the case of tuple) just get encoded and inserted
    Dynamic -> putAbi x

abiHeadSize :: AbiValue -> Int
abiHeadSize x =
  case abiKind (abiValueType x) of
    -- even for dynamic tuples it's just a len() invocation, which is uint256
    Dynamic -> 32
    Static ->
      case x of
        AbiUInt n _  -> roundTo256Bits n
        AbiInt  n _  -> roundTo256Bits n
        AbiBytes n _ -> roundTo256Bits n
        AbiAddress _ -> 32
        AbiBool _    -> 32
        AbiArray _ _ xs -> V.sum (V.map abiHeadSize xs) +
                           V.sum (V.map abiTailSize xs)
        AbiBytesDynamic _ -> 32
        AbiArrayDynamic _ _ -> 32
        AbiString _       -> 32
        AbiTuple v   -> sum (abiHeadSize <$> v) +
                        sum (abiTailSize <$> v) -- pretty sure this is just 0 but w/e

abiTailSize :: AbiValue -> Int
abiTailSize x =
  case abiKind (abiValueType x) of
    Static -> 0
    Dynamic ->
      case x of
        AbiString s -> 32 + roundTo256Bits (BS.length s)
        AbiBytesDynamic s -> 32 + roundTo256Bits (BS.length s)
        AbiArrayDynamic _ xs -> 32 + V.sum (V.map abiValueSize xs)
        AbiArray _ _ xs -> V.sum (V.map abiValueSize xs)
        -- for tuples, special care must be taken to correctly calculate its
        -- encoding length, so we sum the head and tail sizes separately
        AbiTuple v -> sum (headSize <$> v) + sum (abiTailSize <$> v)
        _ -> error "impossible"
  where headSize y = if abiKind (abiValueType y) == Static then abiValueSize y else 32

abiValueSize :: AbiValue -> Int
abiValueSize x =
  case x of
    AbiUInt n _  -> roundTo256Bits n
    AbiInt  n _  -> roundTo256Bits n
    AbiBytes n _ -> roundTo256Bits n
    AbiAddress _ -> 32
    AbiBool _    -> 32
    AbiArray _ _ xs -> V.sum (V.map abiHeadSize xs) +
                       V.sum (V.map abiTailSize xs)
    AbiBytesDynamic xs -> 32 + roundTo256Bits (BS.length xs)
    AbiArrayDynamic _ xs -> 32 + V.sum (V.map abiHeadSize xs) +
                                 V.sum (V.map abiTailSize xs)
    AbiString s -> 32 + roundTo256Bits (BS.length s)
    AbiTuple v  -> sum (abiValueSize <$> v)

roundTo256Bits :: Integral a => a -> a
roundTo256Bits n = 32 * div (n + 255) 256

getBytesWith256BitPadding :: Integral a => a -> Get BS.ByteString
getBytesWith256BitPadding i =
  (BS.pack <$> replicateM n getWord8)
    <* skip (roundTo256Bits n - n)
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
asUInt n f = (\(AbiUInt _ x) -> f (fromIntegral x)) <$> getAbi (AbiUIntType n)

getWord256 :: Get Word256
getWord256 = pack32 8 <$> replicateM 8 getWord32be
