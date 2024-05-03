module Tests.Cheat (cheatTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract', solcV, solved)

import Echidna.Types.Campaign (WorkerType(..))

cheatTests :: TestTree
cheatTests =
  testGroup "Cheatcodes Tests"
    [ testContract' "cheat/ffi.sol" (Just "TestFFI") (Just (> solcV (0,6,0))) (Just "cheat/ffi.yaml") False FuzzWorker
        [ ("echidna_ffi passed", solved "echidna_ffi") ]
    ]
