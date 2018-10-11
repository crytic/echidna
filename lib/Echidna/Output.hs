{-# LANGUAGE DeriveGeneric #-}

module Echidna.Output (
  saveCalls,
  reportPassedTest,
  reportFailedTest
  ) where

import Crypto.Hash.SHA256         (hash)
import System.Directory           (doesFileExist, createDirectoryIfMissing)
import Data.Aeson                 (ToJSON, encode)
import Data.Text                  (Text)
import GHC.Generics

import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)

import qualified Data.ByteString.Base16 as B16 (encode) 

import Echidna.ABI (SolCall, displayAbiSeq, displayAbiCall) 
import Echidna.Event (Events, displayEvents)  

saveCalls :: [SolCall] -> Maybe String -> String -> IO ()
saveCalls _ Nothing     _ = return ()
saveCalls cs (Just dir) p = do
                            let content = displayAbiSeq cs ++ "\n"
                            let filename = dir ++ "/" ++ (hashString $ content) ++ p
                            createDirectoryIfMissing False dir
                            b <- doesFileExist filename
                            if (not b) then (writeFile filename $ content) else return ()
                          where hashString = BS.unpack . B16.encode . hash . BS.pack 


data JsonOutput = JsonOutput {
    propName :: !Text
  , propTrue :: !Bool
  , propCall :: !(Maybe [String])
  , propEvents :: !Events
  , propReducedCall :: !(Maybe [String])
  , propReducedEvents :: !Events
} deriving (Generic, Show)

instance ToJSON JsonOutput

reportPassedTest :: Bool -> Text -> IO () 
reportPassedTest True name = putStrLn $ LBS.unpack $ encode $ JsonOutput { propName = name, propTrue = True, propCall = Nothing, propReducedCall = Nothing, propEvents = [], propReducedEvents = []} 
reportPassedTest False _   = return () 

reportFailedTest :: Bool -> Text -> [SolCall] -> Events -> [SolCall] -> Events -> IO () 
reportFailedTest True name cs es rcs res  = putStrLn $ LBS.unpack $ encode $ JsonOutput { propName = name, 
                                                                                          propTrue = False, 
                                                                                          propCall = Just (map (displayAbiCall) $ reverse cs),
                                                                                          propEvents = es, 
                                                                                          propReducedCall = Just (map (displayAbiCall) $ reverse rcs),
                                                                                          propReducedEvents = res 
                                                                                        }
reportFailedTest False name cs es rcs res = do putStr "Failed property "
                                               print name
                                               putStrLn "Original input:"
                                               putStrLn $ displayAbiSeq cs
                                               putStrLn "Original events:"
                                               putStrLn $ displayEvents es
                                               putStrLn "Reduced input:"
                                               putStrLn $ displayAbiSeq rcs
                                               putStrLn "Reduced events:"
                                               putStrLn $ displayEvents res
 
