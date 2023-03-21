module Echidna.Utility where

import Control.Monad (unless)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Format
import Data.Time.LocalTime
import System.IO (hFlush, stdout)

measureIO :: Bool -> String -> IO b -> IO b
measureIO quiet message action = do
  unless quiet $ do
    time <- timePrefix
    putStr (time <> message  <> "... ") >> hFlush stdout
  t0 <- getCurrentTime
  ret <- action
  t1 <- getCurrentTime
  unless quiet $ putStrLn $ "Done! (" <> show (diffUTCTime t1 t0) <> ")"
  pure ret

timePrefix :: IO String
timePrefix = do
  time <- utcToLocalZonedTime =<< getCurrentTime
  pure $ "[" <> formatTime defaultTimeLocale "%F %T.%2q" time <> "] "
