{-# LANGUAGE DeriveGeneric #-}

module Echidna.Output (
  saveCalls,
  reportPassedTest,
  reportFailedTest
  ) where

import Crypto.Hash.SHA256         (hash)
import System.Directory           (doesFileExist, createDirectoryIfMissing)
import Data.Aeson (ToJSON, encode)
import Data.Text              (Text)
import GHC.Generics

import qualified Data.ByteString.Char8 as BS (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)

import qualified Data.ByteString.Base16 as B16 (encode) 

import Echidna.ABI (SolCall, displayAbiSeq, displayAbiCall) 

saveCalls :: [SolCall] -> Maybe String-> IO ()
saveCalls _ Nothing     = return ()
saveCalls cs (Just dir) = do
                            let content = displayAbiSeq cs ++ "\n"
                            let filename = dir ++ "/" ++ (hashString $ content)
                            createDirectoryIfMissing False dir
                            b <- doesFileExist filename
                            if (not b) then (writeFile filename $ content) else return ()
                          where hashString = BS.unpack . B16.encode . hash . BS.pack 


data JsonOutput = JsonOutput {
    propName :: !Text
  , propTrue :: !Bool
  , propCall :: !(Maybe [String])
  , propReducedCall :: !(Maybe [String])
} deriving (Generic, Show)

instance ToJSON JsonOutput

reportPassedTest :: Bool -> Text -> IO () 
reportPassedTest True name = putStrLn $ LBS.unpack $ encode $ JsonOutput { propName = name, propTrue = True, propCall = Nothing, propReducedCall = Nothing }
reportPassedTest False _   = return () 

reportFailedTest :: Bool -> Text -> [SolCall] -> [SolCall] -> IO () 
reportFailedTest True name cs rcs  = putStrLn $ LBS.unpack $ encode $ JsonOutput { propName = name, propTrue = False, propCall = Just (map displayAbiCall cs), propReducedCall = Just (map displayAbiCall rcs) }
reportFailedTest False name cs rcs = do putStr "Failed property "
                                        print name
                                        putStrLn "Original input:"
                                        putStrLn $ displayAbiSeq cs
                                        putStrLn "Reduced input:"
                                        putStrLn $ displayAbiSeq rcs
