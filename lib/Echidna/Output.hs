{-# LANGUAGE DeriveGeneric #-}

module Echidna.Output (
  --readOutdir,
  --saveCalls,
  updateOutdir,
  syncOutdir,
  reportPassedTest,
  reportFailedTest
  ) where

--import Crypto.Hash.SHA256         (hash)
import Control.Exception          (evaluate)
import System.Directory           (doesFileExist, createDirectoryIfMissing, listDirectory, withCurrentDirectory, makeRelativeToCurrentDirectory)
import System.IO                  (hFlush, stdout)
import Data.Aeson                 (ToJSON, FromJSON, encode, decode)
import Data.Text                  (Text)
import Data.Maybe                 (isJust, fromJust)
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.ByteString.Lazy as LB (readFile)


import Echidna.ABI (SolCall, displayAbiSeq, displayAbiCall) 
import Echidna.Event (Events, displayEvents)  
import Echidna.CoverageInfo (CoverageInfo, CoveragePerInput, findInCover, getCoverId, fromListCover, sizeCover)

--encodeJson :: JsonPropOutput -> IO ()
--encodeJson = putStrLn . LBS.unpack . encode 

data JsonCoverageOutput = JsonCoverageOutput {
    covCall :: !(String)
  , covPrettyCall :: !([String])
  , covInfo :: !(CoverageInfo)
} deriving (Generic, Show)

instance ToJSON JsonCoverageOutput
instance FromJSON JsonCoverageOutput

readCallCoverage :: JsonCoverageOutput -> (CoverageInfo, [SolCall])
readCallCoverage js = (covInfo js, (read (covCall js)) :: ([SolCall]))

readOutdir :: String -> IO CoveragePerInput 
readOutdir d = do
                fs <- listDirectory d 
                xs <- mapM makeRelativeToCurrentDirectory fs
                bs <- withCurrentDirectory d $ mapM LB.readFile xs
                mjs <- return $ map decode bs
                js <- return $ map fromJust $ filter isJust mjs
                covs <- return $ map readCallCoverage js 
                covs' <- evaluate covs
                return $ fromListCover covs

updateOutdir :: CoveragePerInput -> (CoverageInfo,[SolCall]) -> Maybe String -> IO ()
updateOutdir     _      _     Nothing  = return ()
updateOutdir    cov (icov,cs) (Just d) = if (findInCover cov icov) then return ()
                                           else saveCalls (icov, cs) d


syncOutdir :: Maybe String -> CoveragePerInput -> IO CoveragePerInput
syncOutdir Nothing  cov = return cov
syncOutdir (Just d) cov = do cov' <- readOutdir d
                             let diffCov = (sizeCover cov') - (sizeCover cov)
                             putStrLn ("Found " ++ show diffCov ++ " new testcases!")
                             hFlush stdout
                             return cov'

saveCalls :: (CoverageInfo, [SolCall]) -> String -> IO ()
saveCalls (cov,cs) dir = do
                            let content = encodeCallCoverage (cov, cs) --displayAbiSeq cs ++ "\n"
                            let filename = dir ++ "/" ++ (getCoverId cov)
                            createDirectoryIfMissing False dir
                            b <- doesFileExist filename
                            if (not b) then (writeFile filename $ content) else return ()
                          --where hashCov = show . hash . show


encodeCallCoverage :: (CoverageInfo, [SolCall]) -> [Char]
encodeCallCoverage (cov, cs) = encodeJson $ JsonCoverageOutput { covCall = show cs, covPrettyCall = (map (displayAbiCall) $ reverse cs), covInfo = cov } 
                            where encodeJson = LBS.unpack . encode 


data JsonPropOutput = JsonPropOutput {
    propName :: !Text
  , propTrue :: !Bool
  , propCall :: !(Maybe [String])
  , propEvents :: !Events
  , propReducedCall :: !(Maybe [String])
  , propReducedEvents :: !Events
} deriving (Generic, Show)

instance ToJSON JsonPropOutput

reportPassedTest :: Bool -> Text -> IO () 
reportPassedTest True name = encodeJson $ JsonPropOutput { propName = name, propTrue = True, propCall = Nothing, propReducedCall = Nothing, propEvents = [], propReducedEvents = []}
                             where encodeJson = putStrLn . LBS.unpack . encode 
 
reportPassedTest False _   = return () 

reportFailedTest :: Bool -> Text -> [SolCall] -> Events -> [SolCall] -> Events -> IO () 
reportFailedTest True name cs es rcs res  = encodeJson $ JsonPropOutput { propName = name, 
                                                                          propTrue = False, 
                                                                          propCall = Just (map (displayAbiCall) $ reverse cs),
                                                                          propEvents = es, 
                                                                          propReducedCall = Just (map (displayAbiCall) $ reverse rcs),
                                                                          propReducedEvents = res 
                                                                        }
                                            where encodeJson = putStrLn . LBS.unpack . encode 

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
 
