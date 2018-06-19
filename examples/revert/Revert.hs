{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Text                  (Text)
import Control.Monad.State.Strict (evalState)
import Hedgehog hiding            (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import EVM (VM, VMResult(..), Error( Revert ))

import Echidna.Config (withDefaultConfig)
import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = withDefaultConfig $ do
          (v,a,ts) <- loadSolidity "solidity/revert.sol" Nothing
          let prop t = ePropertySeq (`checkRTest` t) a v >>= \x -> return (PropertyName $ show t, x)
          _ <- checkParallel . Group (GroupName "revert.sol") =<< mapM prop ts
          return ()

checkRTest :: VM -> Text -> Bool
checkRTest v t = case evalState (execCall (t, [])) v of
  (VMFailure Revert) -> False
  _                  -> True
