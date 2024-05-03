module Tests.Values (valuesTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContract', solved, solvedLen)

import Echidna.Types.Campaign (WorkerType(..))

valuesTests :: TestTree
valuesTests = testGroup "Value extraction tests"
  [ 
    testContract "values/nearbyMining.sol" Nothing
      [ ("echidna_findNearby passed", solved "echidna_findNearby") ]
    , testContract' "values/smallValues.sol" Nothing Nothing (Just "coverage/test.yaml") False FuzzWorker
      [ ("echidna_findSmall passed", solved "echidna_findSmall") ]
    , testContract "values/constants.sol"    Nothing
      [ ("echidna_found failed",                   solved      "echidna_found")
      , ("echidna_found_large failed",             solved      "echidna_found_large") ]
    , testContract "values/constants2.sol"   Nothing
      [ ("echidna_found32 failed",                 solved      "echidna_found32") ]
    , testContract "values/constants3.sol"   Nothing
      [ ("echidna_found_sender failed",            solved      "echidna_found_sender") ]
    , testContract "values/rconstants.sol"   Nothing
      [ ("echidna_found failed",                   solved      "echidna_found") ]
    , testContract' "values/extreme.sol"   Nothing Nothing (Just "values/extreme.yaml") False FuzzWorker
      [ ("testMinInt8 passed",                   solved      "testMinInt8"),
        ("testMinInt16 passed",                  solved      "testMinInt16"),
        ("testMinInt64 passed",                  solved      "testMinInt32"),
        ("testMinInt128 passed",                 solved      "testMinInt128")
      ]
    , testContract' "values/utf8.sol"   Nothing Nothing (Just "values/extreme.yaml") False FuzzWorker
      [ ("testNonUtf8 passed",                   solved      "testNonUTF8")]
    , testContract' "values/create.sol" (Just "C") Nothing Nothing True FuzzWorker
      [ ("echidna_state failed",                   solved      "echidna_state") ]
    , testContract "values/time.sol"         (Just "values/time.yaml")
      [ ("echidna_timepassed passed",              solved      "echidna_timepassed") ]
    , testContract "values/now.sol"          Nothing
      [ ("echidna_now passed",                     solved      "echidna_now") ]
    , testContract "values/large.sol"        Nothing
      [ ("echidna_large failed",                   solved      "echidna_large") ]
    ,  testContract "values/payable.sol"     Nothing
      [ ("echidna_payable failed",                 solved      "echidna_payable") ]
    , testContract "values/darray.sol"       Nothing
      [ ("echidna_darray passed",                  solved      "echidna_darray")
      , ("echidna_darray didn't shrink optimally", solvedLen 1 "echidna_darray") ]

  ]
