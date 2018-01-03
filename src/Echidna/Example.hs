{-# LANGUAGE LambdaCase #-}

module Echidna.Example where

import Control.Monad.State.Strict hiding (state)
import Control.Lens
import Data.Text (Text)

import qualified Data.Text as T

import EVM
import EVM.ABI
import EVM.Concrete
import EVM.Exec
import EVM.Solidity

import Echidna.Exec

myContract :: Text
myContract = T.unlines [
  "contract Test {",
  "  uint private counter=2**250;",
  "",
  "  function inc(uint val) returns (uint){",
  "    uint tmp = counter;",
  "    counter += val;",
  "    if (tmp > counter) {selfdestruct(0);}",
  "    else {return (counter - tmp);}",
  "  }",
  "  function boom() returns (bool){",
  "    return(true);",
  "  }",
  "}"
  ]

loadSolidity :: Text -> Text -> IO (Maybe VM)
loadSolidity name contents = do
  compiled <- solidity name contents
  case runState exec . vmForEthrunCreation <$> compiled of
    Just (VMSuccess (B bc), vm) ->
      let load = do resetState
                    assign (state . gas) 0xffffffffffffffff
                    loadContract (vm ^. state . contract)
      in return . Just . snd . (runState load) $ execState (replaceCodeOfSelf bc) vm
    _ -> return Nothing

example :: IO ()
example = loadSolidity "Test" myContract >>=
  \case Nothing  -> putStrLn "Compilation Failed"
        (Just c) -> (fuzz 1 10000 [("inc", [AbiUIntType 256])] c worked) >>=
          \case Nothing  -> putStrLn "Tests passed!"
                (Just x) -> putStrLn $ "Tests failed! Counterexample: " ++ show x
  where
    worked v = case v ^. result of (Just (VMSuccess _)) -> return True
                                   _                    -> return False
