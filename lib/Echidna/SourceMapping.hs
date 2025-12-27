module Echidna.SourceMapping where

import Control.Applicative ((<|>))
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Vector qualified as V
import Data.Word (Word16)

import EVM.Dapp (DappInfo(..), findSrc)
import EVM.Expr (maybeLitByteSimp)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Contract(..), ContractCode(..), RuntimeCode(..), W256)

import Echidna.SymExec.Symbolic (forceWord)

-- | Map from contracts' codehashes to their compile-time codehash.
-- This is relevant when the immutables solidity feature is used;
-- when this feature is not used, the map will just end up being an identity map.
-- `CodehashMap` is used in signature map and coverage map lookups.
type CodehashMap = IORef (Map W256 W256)

-- | Lookup a codehash in the `CodehashMap`.
-- In the case that it is not found, find the compile-time codehash and add it to the map.
-- This is done using hevm's `findSrc` function.
lookupCodehash :: CodehashMap -> W256 -> Contract -> DappInfo -> IO W256
lookupCodehash chmap codehash contr dapp = do
  chmapVal <- readIORef chmap
  case Map.lookup codehash chmapVal of
    Just val -> pure val
    Nothing -> do
      -- hevm's `findSrc` doesn't always work, since `SolcContract.immutableReferences` isn't always populated
      let solcContract = findSrc contr dapp <|> findSrcByMetadata contr dapp
          originalCodehash = maybe codehash (.runtimeCodehash) solcContract
      atomicModifyIORef' chmap $ (, ()) . Map.insert codehash originalCodehash
      pure originalCodehash

-- | Given a map from codehash to some values of type `a`, lookup a contract in the map using its codehash.
-- In current use, the `Map W256 a` will be either a `SignatureMap` or a `CoverageMap`.
-- Returns the compile-time codehash, and the map entry if it is found.
lookupUsingCodehash :: CodehashMap -> Contract -> DappInfo -> Map W256 a -> IO (W256, Maybe a)
lookupUsingCodehash chmap contr dapp mapVal =
  ifNotFound codehash $ do
    codehash' <- lookupCodehash chmap codehash contr dapp
    ifNotFound codehash' $
      pure (codehash', Nothing)
  where
    codehash = forceWord contr.codehash
    ifNotFound key notFoundCase = case Map.lookup key mapVal of
      Nothing -> notFoundCase
      Just val -> pure (key, Just val)

-- | Same as `lookupUsingCodehash`, except we add to the map if we don't find anything.
-- The `make` argument is the IO to generate a new element;
-- it is only run if nothing is found in the map.
-- In the case that `make` returns `Nothing`, the map will be unchanged.
-- Returns the map entry, if it is found or generated.
lookupUsingCodehashOrInsert :: CodehashMap -> Contract -> DappInfo -> IORef (Map W256 a) -> IO (Maybe a) -> IO (Maybe a)
lookupUsingCodehashOrInsert chmap contr dapp mapRef make = do
  mapVal <- readIORef mapRef
  (key, valFound) <- lookupUsingCodehash chmap contr dapp mapVal
  case valFound of
    Just val -> pure (Just val)
    Nothing -> applyModification key =<< make
  where
    applyModification _ Nothing = pure Nothing
    applyModification key (Just val) = atomicModifyIORef' mapRef $ modifyFn key val

    -- Take care of multithreaded edge case
    modifyFn key val oldMap = case Map.lookup key oldMap of
      Just val' -> (oldMap, Just val')
      Nothing -> (Map.insert key val oldMap, Just val)

-- | Try to find a SolcContract with a matching bytecode metadata
findSrcByMetadata :: Contract -> DappInfo -> Maybe SolcContract
findSrcByMetadata contr dapp = find compareMetadata (snd <$> Map.elems dapp.solcByHash) where
  compareMetadata solc = contrMeta == Just (getBytecodeMetadata solc.runtimeCode)
  contrMeta = getBytecodeMetadata <$> contrCode
  contrCode = case contr.code of
    (UnknownCode _) -> Nothing
    (InitCode c _) -> Just c
    (RuntimeCode (ConcreteRuntimeCode c)) -> Just c
    (RuntimeCode (SymbolicRuntimeCode c)) -> Just $ BS.pack $ mapMaybe maybeLitByteSimp $ V.toList c

findSrcForReal :: DappInfo -> Contract -> Maybe SolcContract
findSrcForReal dapp contr = findSrc contr dapp <|> findSrcByMetadata contr dapp

-- | Find the position of CBOR length indicator after a metadata prefix.
-- The length indicator is 2 bytes that encode the distance from prefix start to that position.
-- Returns the position of the length indicator (not including the 2 bytes themselves).
findCBORLength :: ByteString -> Int -> Maybe Int
findCBORLength metadata prefixPos = go (prefixPos + 1)
  where
    go currentPos
      | currentPos + 2 > BS.length metadata = Nothing
      | otherwise = case readWord16BE metadata currentPos of
          Nothing -> Nothing
          Just lengthValue ->
            let distanceFromPrefix = currentPos - prefixPos
            in if fromIntegral lengthValue == distanceFromPrefix
               then Just currentPos
               else go (currentPos + 1)
    -- | Read 2 bytes at given position as big-endian Word16
    readWord16BE :: ByteString -> Int -> Maybe Word16
    readWord16BE bs pos
      | pos + 1 < BS.length bs =
          let b1 = fromIntegral (BS.index bs pos) :: Word16
              b2 = fromIntegral (BS.index bs (pos + 1)) :: Word16
          in Just $ (b1 `shiftL` 8) .|. b2
      | otherwise = Nothing

-- | Find the last occurrence of any of the given prefixes in the bytecode
findLastPrefix :: ByteString -> [ByteString] -> Maybe (ByteString, ByteString)
findLastPrefix bs prefixes =
  let findAll prefix = go 0
        where
          go offset = case BS.breakSubstring prefix (BS.drop offset bs) of
            (_, rest) | BS.null rest -> Nothing
            (before, rest) ->
              let pos = offset + BS.length before
                  candidate = (pos, (BS.take pos bs, rest))
              in case go (pos + 1) of
                Nothing -> Just candidate
                Just laterMatch -> Just laterMatch
      allMatches = mapMaybe findAll prefixes
  in if null allMatches
     then Nothing
     else Just $ snd $ maximum allMatches  -- maximum by position

getBytecodeMetadata :: ByteString -> ByteString
getBytecodeMetadata bs =
  case findLastPrefix bs knownBzzrPrefixes of
    Nothing -> bs -- if no metadata is found, return the complete bytecode
    Just (before, fromPrefix) ->
      let prefixPos = BS.length before
      in case findCBORLength bs prefixPos of
        Nothing -> bs -- if no valid CBOR length found, return full bytecode as fallback
        Just lengthPos -> BS.take (lengthPos + 2 - prefixPos) fromPrefix

knownBzzrPrefixes :: [ByteString]
knownBzzrPrefixes =
  -- a1 65 "bzzr0" 0x58 0x20 (solc <= 0.5.8)
  [ BS.pack [0xa1, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20]
  -- a2 65 "bzzr0" 0x58 0x20 (solc >= 0.5.9)
  , BS.pack [0xa2, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20]
  -- a2 65 "bzzr1" 0x58 0x20 (solc >= 0.5.11)
  , BS.pack [0xa2, 0x65, 98, 122, 122, 114, 49, 0x58, 0x20]
  -- a2 64 "ipfs" 0x58 0x22 (solc >= 0.6.0)
  , BS.pack [0xa2, 0x64, 0x69, 0x70, 0x66, 0x73, 0x58, 0x22]
  ]
