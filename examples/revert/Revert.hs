module Main where

import Control.Lens
import Control.Monad

import EVM
import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = do (v,a,_) <- loadSolidity "solidity/revert.sol"
          forever $ fcontract v a

--fcontract :: VM -> [SolSignature] -> IO ()
fcontract v a = do
          res     <- fuzz 2 100 a v worked
          putStr $ maybe "."
                           (("\nTests failed! Counterexample: " ++) . show) res
  where worked c = case c ^. result of (Just (VMSuccess _)) -> return True
                                       (Just (VMFailure Revert)) -> return False
                                       _                    -> return True
