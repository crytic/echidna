module Tests.Seed (seedTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Common (runContract, overrideQuiet)
import Data.Function ((&))
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Campaign (Campaign(..), CampaignConf(..))
import Echidna.Mutator.Corpus (defaultMutationConsts)
import Echidna.Config (defaultConfig)

seedTests :: TestTree
seedTests =
  testGroup "Seed reproducibility testing"
    [ testCase "different seeds" $ assertBool "results are the same" . not =<< same 0 2
    , testCase "same seeds" $ assertBool "results differ" =<< same 0 0
    ]
    where cfg s = defaultConfig
            { campaignConf = CampaignConf 600 False False 20 0 Nothing (Just s) 0.15 Nothing defaultMutationConsts }
            & overrideQuiet
          gen s = do
            camp <- runContract "basic/flags.sol" Nothing (cfg s)
            pure camp._tests
          same s t = (==) <$> gen s <*> gen t
