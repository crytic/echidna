module Tests.Coverage (coverageTests) where

import Test.Tasty (TestTree, testGroup)

import Common (testContract, testContract', passed, countCorpus, checkCoverageUsesCorpusDir, codeUnits, uniqueCodehashes)
import Echidna.Types.Worker (WorkerType(..))

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

  -- A and B share runtime code but not creation code: three creation units
  -- (Main, A, B), two runtime units (Main, A/B), two distinct contracts.
  , testContract' "coverage/shared_runtime.sol" (Just "Main") Nothing (Just "coverage/boolean.yaml") True FuzzWorker
      [ ("creation units keyed by creation code", codeUnits 3 2)
      , ("unique codehashes count owners",        uniqueCodehashes 2)]

  -- Test corpus and coverage directory functionality
  , testContract "basic/revert.sol"              (Just "basic/coverage-test.yaml")
      [ ("corpus count",                           countCorpus 1)]

  -- Test coverage fallback to corpus directory
  , testContract "basic/revert.sol"              (Just "basic/corpus-fallback-test.yaml")
      [ ("uses corpusDir for coverage",           checkCoverageUsesCorpusDir "test-corpus")]

  ]
