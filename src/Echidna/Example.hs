{-# LANGUAGE LambdaCase #-}

module Echidna.Example where

import Control.Monad.State.Strict hiding (state)
import Control.Lens
import Data.Text (Text)

import qualified Data.Text as T

import EVM
import EVM.Concrete
import EVM.Exec
import EVM.Solidity

import Echidna.Exec

myContract :: Text
myContract = T.unlines [
  "contract Test {",
  "  uint private counter=2**50;",
  "",
  "  function inc(uint val) returns (uint){",
  "    uint tmp = counter;",
  "    counter += val;",
  "    if (tmp > counter) {return(counter);}",
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
        (Just c) -> (fuzz 1 10000 [("boom", [])] c worked) >>=
          \case False -> putStrLn "Tests passed!"
                True  -> putStrLn "Tests failed"
  where
    worked v = case v ^. result of (Just (VMSuccess _)) -> return True
                                   x                    -> print x >> return False
