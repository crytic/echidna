module Echidna.Types.CodehashMap where

import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Echidna.Symbolic (forceWord)
import EVM.Dapp (DappInfo, findSrc)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Contract(..), W256)

type CodehashMap = IORef (Map W256 W256)

lookupCodehash :: CodehashMap -> W256 -> Contract -> DappInfo -> IO W256
lookupCodehash chmap codehash contr dapp = do
  chmapVal <- readIORef chmap
  case Map.lookup codehash chmapVal of
    Just val -> pure val
    Nothing -> do
      let originalCodehash = maybe codehash (.runtimeCodehash) (findSrc contr dapp)
      atomicModifyIORef' chmap $ (, ()) . Map.insert codehash originalCodehash
      pure originalCodehash

lookupUsingCodehash :: CodehashMap -> Contract -> DappInfo -> Map W256 a -> IO (W256, Maybe a)
lookupUsingCodehash chmap contr dapp mapVal = do
  ifNotFound codehash $ do
    codehash' <- lookupCodehash chmap codehash contr dapp
    ifNotFound codehash' $ pure (codehash', Nothing)
  where
    codehash = forceWord contr.codehash
    ifNotFound key notFoundCase = case Map.lookup key mapVal of
      Nothing -> notFoundCase
      Just val -> pure (key, Just val)

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

    modifyFn key val oldMap = case Map.lookup key oldMap of
      Just val' -> (oldMap, Just val')
      Nothing -> (Map.insert key val oldMap, Just val)
