module Tests.Config (configTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import Control.Lens ((^.), sans)
import Control.Monad (void)
import Data.Function ((&))

import Echidna.Types.Config (EConfigWithUsage(..), cConf)
import Echidna.Types.Campaign (knownCoverage)
import Echidna.Config (defaultConfig, parseConfig)

configTests :: TestTree
configTests = testGroup "Configuration tests" $
  [ testCase file . void $ parseConfig file | file <- files ] ++
  [ testCase "parse \"coverage: true\"" $ do
      config <- econfig <$> parseConfig "coverage/test.yaml"
      assertCoverage config $ Just mempty
  , testCase "coverage enabled by default" $
      assertCoverage defaultConfig $ Just mempty
  , testCase "default.yaml" $ do
      EConfigWithUsage _ bad unset <- parseConfig "basic/default.yaml"
      assertBool ("unused options: " ++ show bad) $ null bad
      let unset' = unset & sans "seed"
      assertBool ("unset options: " ++ show unset') $ null unset'
  ]
  where files = ["basic/config.yaml", "basic/default.yaml"]
        assertCoverage config value = (config ^. cConf . knownCoverage) @?= value
