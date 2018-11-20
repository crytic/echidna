{-# LANGUAGE LambdaCase, StandaloneDeriving #-}

module Main where

import Prelude hiding (Word)

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState)
import Data.List (nubBy)
import EVM (Query(..), Error(..), VMResult(..), readOp)
import System.Environment (getArgs)

import Echidna.Config
import Echidna.Exec
import Echidna.Solidity

instance Eq Query where
  _ == _ = False

deriving instance Eq Error

interesting :: VMResult -> VMResult -> Bool
interesting (VMFailure a) (VMFailure b) = a == b
interesting (VMFailure _) _             = True
interesting _             _             = False

explain :: VMResult -> String
explain (VMSuccess _) = error "This should never happen"
explain (VMFailure e) = case e of
  UnrecognizedOpcode x -> "Encountered an unimplemented opcode, hevm identifies as: " ++ show (readOp x "")
  SelfDestruction      -> "The contract self-destructed"
  StackUnderrun        -> "The stack underflowed"
  BadJumpDestination   -> "The program jumped somewhere illegal"
  Revert               -> "Encountered a REVERT"
  x                    -> "Unhandled reason! " ++ show x
                          ++ " Please file a ticket on github.com/trailofbits/echidna with this error message"

main :: IO ()
main = let conf m = runReaderT m defaultConfig in getArgs >>= \case
  []     -> putStrLn "need to specify solidity file"
  prog:_ -> do
    (v,_,ts) <- conf $ loadSolidity prog Nothing
    let fails = nubBy interesting $ (\t -> evalState (execCall (t, [])) v) <$> ts
    putStrLn "Failure modes found:"
    mapM_ (putStrLn . ("* " ++) . explain) fails
