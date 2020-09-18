{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Test.Tasty (defaultMain, testGroup)

import System.Directory (withCurrentDirectory)

import Tests.Compile (compilationTests)
import Tests.Config (configTests)
import Tests.Encoding (encodingJSONTests)
import Tests.Integration (integrationTests)
import Tests.Research (researchTests)
import Tests.Seed (seedTests)

main :: IO ()
main = withCurrentDirectory "./examples/solidity" . defaultMain $
         testGroup "Echidna"
           [ configTests
           , compilationTests
           , seedTests
           , integrationTests
           , researchTests
           , encodingJSONTests
           ]

-- Extraction Tests

-- We need to rethink this test
{-
extractionTests :: TestTree
extractionTests = testGroup "Constant extraction/generation testing"
  [ testCase "basic/constants.sol" . flip runReaderT (defaultConfig & sConf . quiet .~ True) $ do
      cs  <- contracts "basic/constants.sol"
      abi <- view _2 <$> loadSpecified Nothing cs
      is  <- evalStateT (replicateM 1000 $ genInteractionsM abi)
                      $ mkGenDict 0.15 (extractConstants cs) []
      forM_ [ ("ints",  ("find",  [AbiInt 256 1447]))
            ("addrs", ("find2", [AbiAddress 0x123]))
            , ("strs",  ("find3", [AbiString "test"]))
            ] $ \(t, c) -> liftIO . assertBool ("failed to extract " ++ t ++ " " ++ show (c,is)) $ elem c is
  ]
-}
