module Tests.Seed (seedTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Common (runContract, overrideQuiet)
import Data.Function ((&))
import Data.IORef (readIORef)
import Echidna.Config (defaultConfig)
import Echidna.Mutator.Corpus (defaultMutationConsts)
import Echidna.Types.Campaign
import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Coverage (CoverageFileType(..))
import Echidna.Types.Test

seedTests :: TestTree
seedTests =
  testGroup "Seed reproducibility testing"
    [ testCase "different seeds" $ assertBool "results are the same" . not =<< same 0 2
    , testCase "same seeds" $ assertBool "results differ" =<< same 0 0
    ]
    where
    cfg s = defaultConfig
      { campaignConf = defaultConfig.campaignConf
        { testLimit = 600
        , stopOnFail = False
        , estimateGas = False
        , seqLen = 20
        , shrinkLimit = 0
        , knownCoverage = Nothing
        , seed = Just s
        , dictFreq = 0.15
        , corpusDir = Nothing
        , mutConsts = defaultMutationConsts
        , coverageFormats = [Txt,Html,Lcov]
        , workers = Nothing
        , serverPort = Nothing
        }
      }
      & overrideQuiet
    gen s = do
      (env, _) <- runContract "basic/flags.sol" Nothing (cfg s) FuzzWorker
      traverse readIORef env.testRefs
    same s t = (\x y -> ((.reproducer) <$> x) == ((.reproducer) <$> y)) <$> gen s <*> gen t
