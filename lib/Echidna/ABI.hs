{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase, RankNTypes, TupleSections, TypeFamilies, TemplateHaskell, DeriveGeneric #-}


module Echidna.ABI (
    SolCall(..)
  , SolSignature
  , encodeAbiCall
  , encodeSig
  , displayAbiCall
  , displayAbiSeq
  , genAbiAddress
  , genAbiArray
  , genAbiArrayDynamic
  , genAbiBool
  , genAbiBytes
  , genAbiBytesDynamic
  , genAbiCall
  , genAbiInt
  , genInteractions
  , genTransactions
  , genAbiString
  , genAbiType
  , genAbiUInt
  , genAbiValue
  , genAbiValueOfType
  , mutateCall
  , mutateCallSeq
  , mutateValue
  , reduceCallSeq
  , prettyPrint
  , fname
  , fargs
  , fsender
  , fvalue
  , parseAsType
) where

import Control.Lens
import Control.Monad         (join, liftM2)
import Control.Monad.Reader  (MonadReader)
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import Data.Bool             (bool)
import Data.DoubleWord       (Word128(..), Word160(..))
import Data.Monoid           ((<>))
import Data.ByteString       (ByteString)
import Data.Text             (Text, unpack)
import Data.Text.Encoding    (encodeUtf8)
import Data.Vector           (Vector, generateM)

import Hedgehog.Internal.Gen (MonadGen)
import GHC.Exts              (IsList(..), Item)
import Hedgehog.Range        (exponential, exponentialFrom, linear, constant, singleton, Range)
import Numeric               (showHex)

import qualified Data.ByteString as BS
import qualified Data.List       as L
import qualified Data.Text       as T
import qualified Hedgehog.Gen    as Gen

import Echidna.Config (Config, addrList, range, sender, payable)

import EVM.ABI
import EVM.Types (Addr(..))

data SolCall = SolCall
  { _fname  :: Text
  , _fargs  :: [AbiValue]
  , _fsender :: Word160
  , _fvalue :: Int 
  } deriving (Eq, Ord, Show, Read)

makeLenses ''SolCall

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

genAbiAddress :: (MonadGen m, MonadReader Config m) => m AbiValue
genAbiAddress = view addrList >>= \case (Just xs) -> fmap (AbiAddress . addressWord160) (Gen.element xs)
                                        Nothing   -> let w64 = Gen.word64 $ constant minBound maxBound in
                                                       fmap AbiAddress . liftM2 Word160 Gen.enumBounded
                                                                           $ liftM2 Word128 w64 w64

genAbiUInt :: MonadGen m => Int -> m AbiValue
genAbiUInt n = AbiUInt n . fromInteger <$> genUInt
               where genUInt = Gen.integral $ exponential 0 $ 2 ^ toInteger n - 1

genAbiInt :: MonadGen m => Int -> m AbiValue
genAbiInt n = AbiInt n . fromInteger <$> genInt
              where genInt = Gen.integral $ exponentialFrom 0 (-1 * 2 ^ toInteger n) (2 ^ (toInteger n - 1))

genAbiBool :: MonadGen m => m AbiValue
genAbiBool = AbiBool <$> Gen.bool

genAbiBytes :: MonadGen m => Int -> m AbiValue
genAbiBytes = liftM2 fmap AbiBytes $ Gen.bytes . singleton

genAbiBytesDynamic :: MonadGen m => m AbiValue
genAbiBytesDynamic = AbiBytesDynamic <$> Gen.bytes (constant 1 2)

genAbiString :: MonadGen m => m AbiValue
genAbiString = let fromRange = fmap AbiString . Gen.utf8 (constant 1 2) in
  Gen.choice $ fromRange <$> [Gen.ascii, Gen.digit, Gen.alpha, Gen.element ['a','b','c'], Gen.unicode]

genStaticAbiType :: MonadGen m => m AbiType
genStaticAbiType = go (16 :: Int) where
  go n = Gen.choice $ [ AbiUIntType <$> genSize
                      , AbiIntType <$> genSize
                      , pure AbiAddressType
                      , pure AbiBoolType
                      , AbiBytesType <$> Gen.enum 1 2
                      ] ++ [AbiArrayType <$> Gen.enum 0 256 <*> go (n - 1) | n > 0] 

genAbiType :: MonadGen m => m AbiType
genAbiType = Gen.choice [ pure AbiBytesDynamicType
                        , pure AbiStringType
                        , AbiArrayDynamicType <$> genStaticAbiType
                        , genStaticAbiType
                        ]

genVecOfType :: (MonadReader Config m, MonadGen m) => AbiType -> Range Int -> m (Vector AbiValue)
genVecOfType t r = do
  s <- Gen.integral r
  generateM s $ \_ -> case t of
    AbiUIntType    n    -> genAbiUInt n
    AbiIntType     n    -> genAbiInt n
    AbiAddressType      -> genAbiAddress
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
                         , genAbiAddress
                         , genAbiBool
                         , genAbiBytes =<< Gen.enum 1 2
                         , genAbiBytesDynamic
                         , genAbiString
                         , genAbiArrayDynamic =<< genAbiType
                         , join $ liftM2 genAbiArray (Gen.enum 0 256) genAbiType
                         ]

genAbiValueOfType :: (MonadReader Config m, MonadGen m) => AbiType -> m AbiValue
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

genMsgValue ::  (MonadReader Config m, MonadGen m) => m Int
genMsgValue = Gen.integral $ exponential 0 $ 2 ^ (32 :: Integer) - 1


genAbiCall :: (MonadReader Config m, MonadGen m) => SolSignature -> m SolCall
genAbiCall (s,ts) = view sender >>= (\addrs -> 
                    (view payable) >>= (\pays -> do
                                                 addr <- Gen.element addrs
                                                 cs <- mapM genAbiValueOfType ts 
                                                 v <- if s `elem` pays then genMsgValue else return 0
                                                 return (SolCall s cs (addressWord160 addr) v)
                                      ))

encodeAbiCall :: SolCall -> ByteString
encodeAbiCall sc = abiCalldata (view fname sc) $ fromList (view fargs sc)

displayAbiCall :: SolCall -> String
displayAbiCall sc = unpack (view fname sc) ++ "(" ++ L.intercalate "," (map prettyPrint (view fargs sc)) ++ ") by " ++ (showHex ( (view fsender sc)) "") ++
                    if (view fvalue sc) > 0 then " with " ++ show (view fvalue sc) else "" 

--instance Show SolCall where  
--  show = displayAbiCall
 
displayAbiSeq :: [SolCall] -> String
displayAbiSeq = {- ("Call sequence: " ++) .-} L.intercalate "\n" {-"               "-} . (map displayAbiCall)

-- genInteractions generates a function call from a list of type signatures of
-- the form (Function name, [arg0 type, arg1 type...])
genInteractions :: (MonadReader Config m, MonadGen m) => [SolSignature] -> m SolCall
genInteractions ls = genAbiCall =<< Gen.element ls

genTransactions :: (MonadReader Config m, MonadGen m) => Int -> [SolSignature] -> m [SolCall]
genTransactions n =  Gen.list (linear 1 n) . genInteractions
 


type Listy t a = (IsList (t a), Item (t a) ~ a)

switchElem :: (Listy t a, MonadGen m) => m a -> t a -> m (t a)
switchElem g t = let l = toList t; n = length l in do
  if n == 0 then return t 
  else do
        i <- Gen.element [0..n]
        x <- g
        return . fromList $ take i l <> [x] <> drop (i+1) l

changeChar :: MonadGen m => ByteString -> m ByteString
changeChar = fmap BS.pack . switchElem Gen.enumBounded . BS.unpack

--addBS :: MonadGen m => ByteString -> m ByteString
--addBS b = Gen.element [(<> b), (b <>)] <*> Gen.utf8 (constant 0 (256 - BS.length b)) Gen.unicode

--dropBS :: MonadGen m => ByteString -> m ByteString
--dropBS b = Gen.choice [ BS.drop <$> Gen.element [1..BS.length b]   <*> pure b
--                      , BS.take <$> Gen.element [0..BS.length b-1] <*> pure b
--                      ]

changeDynamicBS :: MonadGen m => ByteString -> m ByteString
changeDynamicBS b = Gen.choice $ [changeChar] {- addBS, dropBS]-} <&> ($ b)

changeNumber :: (Enum a, Integral a, MonadGen m) => a -> m a
changeNumber n = let x = fromIntegral n :: Integer in fromIntegral . (+ x) <$> Gen.element [-10..10]

{-
changeList :: (Listy t a, MonadGen m) => m (t a) -> m a -> t a -> m (t a)
changeList g0 g1 x = case toList x of
  [] -> g0
  l -> Gen.choice [ Gen.element [(<> l), (l <>)] <*> fmap toList g0
                  , drop <$> Gen.element [1..length l] <*> pure l
                  , take <$> Gen.element [0..length l-1] <*> pure l
                  , switchElem g1 l
                  ] <&> fromList
-}

newOrMod ::  MonadGen m => m AbiValue -> (a -> AbiValue) -> m a -> m AbiValue
newOrMod m f n = Gen.choice [m, f <$> n]

mutateValue :: (MonadReader Config m, MonadGen m) => AbiValue -> m AbiValue
mutateValue (AbiUInt s n) =
  newOrMod (genAbiUInt s)         (AbiUInt s)         (changeNumber n)
mutateValue (AbiInt s n) =
  newOrMod (genAbiInt s)          (AbiInt s)          (changeNumber n)
mutateValue (AbiAddress a) = return $ AbiAddress a
mutateValue (AbiBool _) = genAbiBool
mutateValue (AbiBytes s b) =
  newOrMod (genAbiBytes s)        (AbiBytes s)        (changeChar b)
mutateValue (AbiBytesDynamic b) =
  newOrMod genAbiBytesDynamic     AbiBytesDynamic     (changeDynamicBS b)
mutateValue (AbiString b) =
  newOrMod genAbiString           AbiString           (changeDynamicBS b)
mutateValue (AbiArrayDynamic t a) = return (AbiArrayDynamic t a) 
  --let g0 = genVecOfType t (constant 0 (256 - length a)); g1 = genAbiValueOfType t in
  --newOrMod (genAbiArrayDynamic t) (AbiArrayDynamic t) (changeList g0 g1 a)
mutateValue (AbiArray s t a) =  return (AbiArray s t a)  
  --newOrMod (genAbiArray s t)      (AbiArray s t)      (switchElem (genAbiValueOfType t) a)

changeOrId :: (Traversable t, MonadGen m) => (a -> m a) -> t a -> m (t a)
changeOrId f = mapM $ (Gen.element [f, pure] >>=) . (&)

mutateCall :: (MonadReader Config m, MonadGen m) => SolCall -> m SolCall
mutateCall sc = (\args -> SolCall (view fname sc) args (view fsender sc) (view fvalue sc)) <$> changeOrId mutateValue (view fargs sc)

addCall :: (MonadReader Config m, MonadGen m) => [SolSignature] -> [SolCall] -> m [SolCall]
addCall _ []  = undefined 
addCall ts cs = do n <- Gen.element [0 .. length cs - 1]
                   e <- genInteractions ts
                   return (take n cs ++ [e] ++ drop n cs)  

dropCall :: (MonadGen m) => [SolCall] -> m [SolCall]
dropCall cs = do n <- Gen.element [0 .. length cs - 1]
                 Gen.element [ take n cs,  drop (n+1) cs, (take n cs) ++ drop (n+1) cs]

mutateCallSeq :: (MonadReader Config m, MonadGen m) => [SolSignature] -> [SolCall] -> m [SolCall]
mutateCallSeq ts cs = view range >>= (\n ->
                                      case (length cs) of 
                                        1          -> Gen.choice [changeOrId mutateCall cs, addCall ts cs]
                                        l | l == n -> Gen.choice [changeOrId mutateCall cs, dropCall cs]
                                        _          -> Gen.choice [changeOrId mutateCall cs, dropCall cs, addCall ts cs] 
                                     )

-- Only for reduction
reduceCall :: (MonadGen m) => SolCall -> m SolCall
reduceCall sc = (\args -> SolCall (view fname sc) args (view fsender sc) (view fvalue sc)) <$> mapM reduceValue (view fargs sc)

reduceCallSeq :: (MonadGen m) => [SolCall] -> m [SolCall]
reduceCallSeq cs = if (length cs) <= 1 then Gen.choice [changeOrId reduceCall cs] 
                                       else Gen.frequency [(1,changeOrId reduceCall cs), (1,dropCall cs)] 

reduceValue :: (MonadGen m) => AbiValue -> m AbiValue
reduceValue (AbiUInt s n) = (AbiUInt s) <$> reduceNumber n
reduceValue (AbiInt s n) = (AbiInt s) <$> reduceNumber n
reduceValue (AbiAddress a) = return $ AbiAddress a
reduceValue (AbiBool _) = return $ AbiBool False
reduceValue (AbiBytes s b) = return $ AbiBytes s b
reduceValue (AbiBytesDynamic b) = return $ AbiBytesDynamic b
reduceValue (AbiString b) = return $ AbiString b
reduceValue (AbiArrayDynamic t a) = return (AbiArrayDynamic t a) 
reduceValue (AbiArray s t a) =  return (AbiArray s t a)  

reduceNumber :: (Enum a, Integral a, MonadGen m) => a -> m a
reduceNumber 0 = return 0
reduceNumber n = if (n>0) then Gen.integral $ exponential 0 (n `div` 2) else Gen.integral $ exponentialFrom 0 (n `div` 2) 0

parseAsType :: AbiType -> Value -> Parser AbiValue
parseAsType t v = case (t, v) of
  (AbiUIntType n, Number x) ->
      case toBoundedInteger x of
          Nothing -> fail $ "cannot parse " ++ show x ++ " as " ++ show t
          Just i  -> pure (AbiUInt n i)
--          Just i  -> if 0 <= i && i < 2^n
--                     then pure (AbiUInt n i)
--                     else fail $ show i ++ " too wide for " ++ show t
  (AbiIntType n, Number x) ->
      case toBoundedInteger x of
          Nothing -> fail $ "cannot parse " ++ show x ++ " as " ++ show t
          Just i  -> pure (AbiInt n i)
--          Just i  -> if -2^(n-1) <= i && i < 2^(n-1)
--                     then pure (AbiInt n i)
--                     else fail $ show i ++ " too wide for " ++ show t
  (AbiAddressType, Number x) ->
      case toBoundedInteger x of
         Nothing   -> fail "cannot parse float as address"
         Just addr -> pure (AbiAddress addr)
  (AbiBytesType n, String s) -> let bs = encodeUtf8 s in
                                if BS.length bs == n
                                then pure (AbiBytes n bs)
                                else fail $ show bs ++ " wrong size for " ++ show t
  (AbiBytesDynamicType, String s) ->
      let bs = encodeUtf8 s in pure (AbiBytes (BS.length bs) bs)

  (AbiStringType, String s) -> pure (AbiString $ encodeUtf8 s)

  (AbiArrayType n elemType, Array a) ->
      if length a == n
      then AbiArray n elemType <$> traverse (parseAsType elemType) a
      else fail $ show a ++ " wrong size for " ++ show t

  (AbiArrayDynamicType elemType, Array a) ->
      AbiArrayDynamic elemType <$> traverse (parseAsType elemType) a

  _ -> typeMismatch (show t) v
