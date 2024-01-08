module Echidna.Types.CodehashMap where

import Data.IORef (IORef, readIORef, atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Echidna.Symbolic (forceWord)
import EVM.Dapp (DappInfo, findSrc)
import EVM.Solidity (SolcContract(..))
import EVM.Types (Contract(..), W256)

type CodehashMap = IORef (Map W256 W256)

lookupCodehash :: CodehashMap -> W256 -> Contract -> DappInfo -> IO W256
lookupCodehash chmap codehash contr dapp = Map.lookup codehash <$> readIORef chmap >>= \case
  Just val -> pure val
  Nothing -> do
    let originalCodehash = fromMaybe codehash $ (.runtimeCodehash) <$> findSrc contr dapp
    atomicModifyIORef' chmap $ (, ()) . Map.insert codehash originalCodehash
    pure originalCodehash

lookupUsingCodehash :: CodehashMap -> Contract -> DappInfo -> IORef (Map W256 a) -> IO (Maybe a) -> IO (Maybe a)
lookupUsingCodehash chmap contr dapp mapRef make = do
  mapVal <- readIORef mapRef
  ifNotFound codehash mapVal $ do
    codehash' <- lookupCodehash chmap codehash contr dapp
    ifNotFound codehash' mapVal $ do
      toInsert <- make
      applyModification codehash' toInsert
  where
    codehash = forceWord contr.codehash
    ifNotFound key mapVal notFoundCase = case (Map.lookup key mapVal) of
      Nothing -> notFoundCase
      Just val -> pure (Just val)

    applyModification _ Nothing = pure Nothing
    applyModification key (Just val) = atomicModifyIORef' mapRef $ modifyFn key val

    modifyFn key val oldMap = case (Map.lookup key oldMap) of
      Just val' -> (oldMap, Just val')
      Nothing -> (Map.insert key val oldMap, Just val)
