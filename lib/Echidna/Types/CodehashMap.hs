module Echidna.Types.CodehashMap where

import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Echidna.Symbolic (forceWord)
import EVM.Dapp (DappInfo, findSrc)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Contract(..), W256)

-- | Map from contracts' codehashes to their "real" (compile-time) codehash.
-- This is relevant when the immutables solidity feature is used;
-- when this feature is not used, the map will just end up being an identity map.
-- `CodehashMap` is used in signature map and coverage map lookups.
type CodehashMap = IORef (Map W256 W256)

-- | Lookup a codehash in the `CodehashMap`.
-- In the case that it's not found, find the "real" (compile-time) codehash and add it to the map.
-- This is done using hevm's `findSrc` function.
lookupCodehash :: CodehashMap -> W256 -> Contract -> DappInfo -> IO W256
lookupCodehash chmap codehash contr dapp = do
  chmapVal <- readIORef chmap
  case Map.lookup codehash chmapVal of
    Just val -> pure val
    Nothing -> do
      let originalCodehash = maybe codehash (.runtimeCodehash) (findSrc contr dapp)
      atomicModifyIORef' chmap $ (, ()) . Map.insert codehash originalCodehash
      pure originalCodehash

-- | Given a map from codehash to some values of type `a`, lookup a contract in the map using its codehash.
-- In current use, the `Map W256 a` will be either a `SignatureMap` or a `CoverageMap`.
-- Returns the "real" codehash, and the map entry if it is found.
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
