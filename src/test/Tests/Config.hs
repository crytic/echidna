module Tests.Config (configTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure)

import Optics.Core (sans)
import Control.Monad (void)
import Data.Function ((&))
import Data.Yaml qualified as Y

import Echidna.Types.Config (EConfigWithUsage(..), EConfig(..))
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Tx (TxConf(..))
import Echidna.Config (defaultConfig, parseConfig)
import Data.Maybe (isJust, isNothing)

configTests :: TestTree
configTests = testGroup "Configuration tests" $
  [ testCase file . void $ parseConfig file | file <- files ] ++
  [ testCase "parse \"coverage: true\"" $ do
      config <- (.econfig) <$> parseConfig "coverage/test.yaml"
      assertBool "" $ isJust config.campaignConf.knownCoverage
  , testCase "coverage enabled by default" $
      assertBool "" $ isJust defaultConfig.campaignConf.knownCoverage
  , testCase "parse corpusDir" $ do
      config <- (.econfig) <$> parseConfig "research/bran_bar.yaml"
      assertBool "" $ config.campaignConf.corpusDir == Just "corpus"
  , testCase "parse coverageDir" $ do
      config <- (.econfig) <$> parseConfig "basic/coverage-test.yaml"
      assertBool "" $ config.campaignConf.coverageDir == Just "coverage-reports"
  , testCase "corpusDir and coverageDir independent" $ do
      config <- (.econfig) <$> parseConfig "basic/coverage-test.yaml"
      assertBool "corpusDir should be set" $ config.campaignConf.corpusDir == Just "corpus-data"
      assertBool "coverageDir should be set" $ config.campaignConf.coverageDir == Just "coverage-reports"
  , testCase "coverageDir fallback to corpusDir" $ do
      config <- (.econfig) <$> parseConfig "basic/corpus-fallback-test.yaml"
      assertBool "corpusDir should be set" $ config.campaignConf.corpusDir == Just "test-corpus"
      assertBool "coverageDir should not be set" $ isNothing (config.campaignConf.coverageDir)
  , testCase "corpusDir defaults to Nothing" $
      assertBool "" $ isNothing (defaultConfig.campaignConf.corpusDir)
  , testCase "coverageDir defaults to Nothing" $
      assertBool "" $ isNothing (defaultConfig.campaignConf.coverageDir)
  , testCase "default.yaml" $ do
      EConfigWithUsage _ bad unset <- parseConfig "basic/default.yaml"
      assertBool ("unused options: " ++ show bad) $ null bad
      let unset' = unset & sans "seed"
      assertBool ("unset options: " ++ show unset') $ null unset'
  , testCase "W256 decoding" $ do
      let maxW256  = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
          overW256 = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0"
      case Y.decodeEither' ("maxGasprice: " <> maxW256) of
        Right (c :: EConfigWithUsage) | c.econfig.txConf.maxGasprice == maxBound -> pure ()
        Right _ -> assertFailure "wrong value decoded"
        Left e -> assertFailure $ "unexpected decoding error: " <> show e
      case Y.decodeEither' ("maxGasprice: " <> overW256) of
        Right (_ :: EConfigWithUsage) -> assertFailure "should not decode"
        Left _ -> pure ()
  ]
  where files = ["basic/config.yaml", "basic/default.yaml", "basic/coverage-test.yaml", "basic/corpus-fallback-test.yaml"]
