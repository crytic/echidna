{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Test.Tasty (defaultMain, testGroup)
import System.Directory (withCurrentDirectory)
import Tests.Assertion (assertionTests)
import Tests.Compile (compilationTests)
import Tests.Config (configTests)
import Tests.Encoding (encodingJSONTests)
import Tests.Integration (integrationTests)
import Tests.Research (researchTests)
import Tests.Seed (seedTests)

main :: IO ()
main = withCurrentDirectory "./tests/solidity" . defaultMain $
         testGroup "Echidna"
           [ configTests
           , compilationTests
           , seedTests
           , integrationTests
           , assertionTests
           , researchTests
           , encodingJSONTests
           ]
