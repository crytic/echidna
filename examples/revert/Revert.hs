module Main where

import Data.Text                  (Text)
import Control.Monad.Reader       (runReader)
import Control.Monad.State.Strict (evalState)
import Hedgehog hiding            (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import EVM (VM, VMResult(..), Error( Revert ))

import Echidna.Config (defaultConfig)
import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = do (v,a,ts) <- loadSolidity "solidity/revert.sol" Nothing Nothing
          let prop t = (PropertyName $ show t, runReader (ePropertySeq (`checkRTest` t) a v) defaultConfig)
          _ <- checkParallel . Group (GroupName "revert.sol") $ map prop ts
          return ()

checkRTest :: VM -> Text -> Bool
checkRTest v t = case evalState (execCall (t, [])) v of
  (VMFailure Revert) -> False
  _                  -> True
