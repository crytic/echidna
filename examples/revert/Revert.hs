module Main where

import Data.Text                  (Text)
import Control.Monad.State.Strict (evalState)
import Hedgehog hiding            (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import EVM (VM, VMResult(..), Error( Revert ))

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = do (v,a,ts) <- loadSolidity "solidity/revert.sol" Nothing
          let prop t = (PropertyName $ show t, ePropertySeq v a (`checkRTest` t) 10)
          _ <- checkParallel . Group (GroupName "revert.sol") $ map prop ts
          return ()

checkRTest :: VM -> Text -> Bool
checkRTest v t = case evalState (execCall (t, [], 0, 0)) v of
  (VMFailure Revert) -> False
  _                  -> True
