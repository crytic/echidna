module Tests.Coverage (coverageTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, passed, countCorpus)

coverageTests :: TestTree
coverageTests = testGroup "Coverage tests"
  [
      -- single.sol is really slow and kind of unstable. it also messes up travis.
     -- testContract "coverage/single.sol"    (Just "coverage/test.yaml")
     -- [ ("echidna_state failed",                   solved      "echidna_state") ]
     -- testContract' "coverage/multi.sol" Nothing Nothing (Just "coverage/test.yaml") False False
     -- [ ("echidna_state3 failed",                  solved      "echidna_state3") ]
      testContract "coverage/boolean.sol"       (Just "coverage/boolean.yaml")
      [ ("echidna_true failed",                    passed     "echidna_true")
      , ("unexpected corpus count ",               countCorpus 1)]

  ]
