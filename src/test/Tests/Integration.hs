module Tests.Integration (integrationTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContractV, solcV, testContract', checkConstructorConditions, passed, solved, solvedLen, solvedWith, solvedWithout, gasInRange)
import Data.Functor ((<&>))
import Data.Text (unpack)
import Echidna.Types.Campaign (WorkerType(..))
import Echidna.Types.Tx (TxCall(..))
import EVM.ABI (AbiValue(..))

integrationTests :: TestTree
integrationTests = testGroup "Solidity Integration Testing"
  [ testContract "basic/true.sol" Nothing
      [ ("echidna_true failed", passed "echidna_true") ]
  , testContract "basic/flags.sol" Nothing
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  solved      "echidna_sometimesfalse")
      , ("echidna_sometimesfalse didn't shrink optimally", solvedLen 2 "echidna_sometimesfalse")
      ]
  , testContract "basic/flags.sol" (Just "basic/whitelist.yaml")
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  passed      "echidna_sometimesfalse")
      ]
  , testContract "basic/flags.sol" (Just "basic/whitelist_all.yaml")
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  solved      "echidna_sometimesfalse")
      ]
  , testContract "basic/flags.sol" (Just "basic/blacklist.yaml")
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  passed      "echidna_sometimesfalse")
      ]
  , testContract "basic/revert.sol" Nothing
      [ ("echidna_fails_on_revert passed", solved "echidna_fails_on_revert")
      , ("echidna_fails_on_revert didn't shrink to one transaction",
         solvedLen 1 "echidna_fails_on_revert")
      , ("echidna_revert_is_false didn't shrink to f(-1, 0x0, 0xdeadbeef)",
         solvedWith (SolCall ("f", [AbiInt 256 (-1), AbiAddress 0, AbiAddress 0xdeadbeef])) "echidna_fails_on_revert")
      ]
  , testContract "basic/multisender.sol" (Just "basic/multisender.yaml") $
      [ ("echidna_all_sender passed",                      solved             "echidna_all_sender")
      , ("echidna_all_sender didn't shrink optimally",     solvedLen 3        "echidna_all_sender")
      ] ++ (["s1", "s2", "s3"] <&> \n ->
        ("echidna_all_sender solved without " ++ unpack n, solvedWith (SolCall (n, [])) "echidna_all_sender"))
  , testContract "basic/memory-reset.sol" Nothing
      [ ("echidna_memory failed",                  passed      "echidna_memory") ]
  , testContract "basic/contractAddr.sol" (Just "basic/contractAddr.yaml")
      [ ("echidna_address failed",                 passed      "echidna_address") ]
  , testContractV "basic/balance.sol"     (Just (< solcV (0,8,0)))  (Just "basic/balance.yaml")
      [ ("echidna_balance failed",                 passed      "echidna_balance")
      , ("echidna_balance_new failed",             passed      "echidna_balance_new")
      , ("echidna_low_level_call failed",          passed      "echidna_low_level_call")
      , ("echidna_no_magic failed",                passed      "echidna_no_magic")
      ]
  , testContract "basic/library.sol"      (Just "basic/library.yaml")
      [ ("echidna_library_call failed",            solved      "echidna_library_call")
      , ("echidna_valid_timestamp failed",         passed      "echidna_valid_timestamp")
      ]
  , testContractV "basic/fallback.sol"   (Just (< solcV (0,6,0))) Nothing
      [ ("echidna_fallback failed",                solved      "echidna_fallback") ]
  , testContract "basic/push_long.sol" (Just "basic/push_long.yaml")
      [ ("test_long_5 passed",                     solvedWithout NoCall "test_long_5")]
  , testContract "basic/propGasLimit.sol" (Just "basic/propGasLimit.yaml")
      [ ("echidna_runForever passed",              solved      "echidna_runForever") ]
  , testContract "basic/delay.sol"        Nothing
      [ ("echidna_block_number passed",            solved    "echidna_block_number")
      , ("echidna_timestamp passed",               solved    "echidna_timestamp") ]
  , testContractV "basic/immutable.sol"    (Just (>= solcV (0,6,0))) Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContractV "basic/immutable-2.sol"    (Just (>= solcV (0,6,0))) Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "basic/construct.sol"    Nothing
      [ ("echidna_construct passed",               solved      "echidna_construct") ]
  , testContract "basic/gasprice.sol"     (Just "basic/gasprice.yaml")
      [ ("echidna_state passed",                   solved      "echidna_state") ]
  , testContract' "basic/allContracts.sol" (Just "B") Nothing (Just "basic/allContracts.yaml") True FuzzWorker
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "basic/array-mutation.sol"   Nothing
      [ ("echidna_mutated passed",                 solved      "echidna_mutated") ]
  , testContract "basic/darray-mutation.sol"  Nothing
      [ ("echidna_mutated passed",                 solved      "echidna_mutated") ]
  , testContract "basic/gasuse.sol"       (Just "basic/gasuse.yaml")
      [ ("echidna_true failed",                    passed     "echidna_true")
      , ("g gas estimate wrong",                   gasInRange "g" 130000 40000000)
      , ("f_close1 gas estimate wrong",            gasInRange "f_close1" 400 2000)
      , ("f_open1 gas estimate wrong",             gasInRange "f_open1"  18000 23000)
      , ("push_b gas estimate wrong",              gasInRange "push_b"   39000 45000)
      ]
  , testContract "basic/gaslimit.sol"  Nothing
      [ ("echidna_gaslimit passed",                passed      "echidna_gaslimit") ]
  ,  testContractV "basic/killed.sol"      (Just (< solcV (0,8,0))) Nothing
      [ ("echidna_still_alive failed",             solved      "echidna_still_alive") ]
  ,  checkConstructorConditions "basic/codesize.sol"
      "invalid codesize"
  , testContractV "basic/eip-170.sol" (Just (>= solcV (0,5,0))) (Just "basic/eip-170.yaml")
      [ ("echidna_test passed",                    passed      "echidna_test") ]
  , testContract' "basic/deploy.sol" (Just "Test") Nothing (Just "basic/deployContract.yaml") True FuzzWorker
      [ ("test passed",                    solved     "test") ]
  , testContract' "basic/deploy.sol" (Just "Test") Nothing (Just "basic/deployBytecode.yaml") True FuzzWorker
      [ ("test passed",                    solved     "test") ]
  , testContract "basic/flags.sol" (Just "basic/etheno-query-error.yaml")
      [] -- Just check if the etheno config does not crash Echidna
  ]
