module Main where

import Control.Lens

import EVM

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = do (v,a,_) <- loadSolidity "solidity/test.sol"
          res     <- fuzz 1 10000 a v worked
          putStrLn $ maybe "Tests passed!"
                           (("Tests failed! Counterexample: " ++) . show) res
  where worked c = case c ^. result of (Just (VMSuccess _)) -> return True
                                       _                    -> return False
