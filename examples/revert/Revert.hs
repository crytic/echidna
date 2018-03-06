module Main where

import Hedgehog hiding (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = do (v,a,ts) <- loadSolidity "solidity/revert.sol"
          let prop t = (PropertyName $ show t, ePropertySeq v a (`checkRTest` t) 10)
          _ <- checkParallel . Group (GroupName "revert.sol") $ map prop ts
          return ()
