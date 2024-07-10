{-# LANGUAGE TemplateHaskell #-}

module Echidna.CatchMVar where

import Control.Exception
import Language.Haskell.TH

-- https://tech.fpcomplete.com/blog/2018/05/pinpointing-deadlocks-in-haskell/
catchMVar :: String -> IO a -> IO a
catchMVar msg action =
  action `catches`
  [ Handler (\exc@BlockedIndefinitelyOnMVar -> putStrLn ("[MVar]: Error at " ++ msg) >> throwIO exc)
  , Handler (\exc@BlockedIndefinitelyOnSTM -> putStrLn ("[STM]: Error at " ++ msg) >> throwIO exc)
  ]

printLocation :: Q Exp
printLocation = do
  loc <- location
  let
    fname = loc_filename loc
    line = fst $ loc_start loc
    msg = fname ++ ":" ++ show line
  litE $ stringL msg

catchMVarTempl :: Q Exp
catchMVarTempl = [| catchMVar $printLocation |]

putMVar_ :: Q Exp
putMVar_ = [| ($catchMVarTempl .) . putMVar |]

tryPutMVar_ :: Q Exp
tryPutMVar_ = [| ($catchMVarTempl .) . tryPutMVar |]

takeMVar_ :: Q Exp
takeMVar_ = [| $catchMVarTempl . takeMVar |]

readMVar_ :: Q Exp
readMVar_ = [| $catchMVarTempl . readMVar |]

writeChan_ :: Q Exp
writeChan_ = [| ($catchMVarTempl .) . writeChan |]

readChan_ :: Q Exp
readChan_ = [| $catchMVarTempl . readChan |]

writeBChan_ :: Q Exp
writeBChan_ = [| ($catchMVarTempl .) . writeBChan |]

writeBChanNonBlocking_ :: Q Exp
writeBChanNonBlocking_ = [| ($catchMVarTempl .) . writeBChanNonBlocking |]
