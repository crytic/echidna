{-# LANGUAGE FlexibleContexts #-}

module Main where

import Hedgehog hiding            (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Echidna.Config (withDefaultConfig)
import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = withDefaultConfig $ do
          (v,a,ts) <- loadSolidity "solidity/revert.sol" Nothing
          let prop t = ePropertySeq (`checkRevertTest` t) a v >>= \x -> return (PropertyName $ show t, x)
          _ <- checkParallel . Group (GroupName "revert.sol") =<< mapM prop ts
          return ()
