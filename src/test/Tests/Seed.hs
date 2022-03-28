module Tests.Seed (seedTests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Common (runContract)
import Control.Lens (view, (.~))
import Control.Monad (liftM2)
import Data.Function ((&))
import Echidna.Types.Config (sConf, cConf)
import Echidna.Types.Solidity (quiet)
import Echidna.Types.Campaign (CampaignConf(..), tests)
import Echidna.Mutator.Corpus (defaultMutationConsts)
import Echidna.Config (defaultConfig)

seedTests :: TestTree
seedTests =
  testGroup "Seed reproducibility testing"
    [ testCase "different seeds" $ assertBool "results are the same" . not =<< same 0 2
    , testCase "same seeds" $ assertBool "results differ" =<< same 0 0
    ]
    where cfg s = defaultConfig & sConf . quiet .~ True
                                & cConf .~ CampaignConf 600 False False 20 0 Nothing (Just s) 0.15 Nothing defaultMutationConsts
          gen s = view tests <$> runContract "basic/flags.sol" Nothing (cfg s)
          same s t = liftM2 (==) (gen s) (gen t)
