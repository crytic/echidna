module Echidna.SourceMapping where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Vector qualified as V
import Echidna.Symbolic (forceWord)
import EVM.Dapp (DappInfo(..), findSrc)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Contract(..), ContractCode(..), RuntimeCode(..), W256, maybeLitByte)

-- | Map from contracts' codehashes to their compile-time codehash.
-- This is relevant when the immutables solidity feature is used;
-- when this feature is not used, the map will just end up being an identity map.
-- `CodehashMap` is used in signature map and coverage map lookups.
type CodehashMap = IORef (Map W256 W256)

-- | Lookup a codehash in the `CodehashMap`.
-- In the case that it's not found, find the compile-time codehash and add it to the map.
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
    (RuntimeCode (SymbolicRuntimeCode c)) -> Just $ BS.pack $ mapMaybe maybeLitByte $ V.toList c

getBytecodeMetadata :: ByteString -> ByteString
getBytecodeMetadata bs =
  let stripCandidates = flip BS.breakSubstring bs <$> knownBzzrPrefixes in
    case find ((/= mempty) . snd) stripCandidates of
      Nothing     -> bs -- if no metadata is found, return the complete bytecode
      Just (_, m) -> m

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
