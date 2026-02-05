import System.Directory (withCurrentDirectory)
import Test.Tasty (defaultMain, testGroup)
import Tests.ABIv2 (abiv2Tests)
import Tests.Assertion (assertionTests)
import Tests.Cheat (cheatTests)
import Tests.Compile (compilationTests)
import Tests.Config (configTests)
import Tests.Coverage (coverageTests)
import Tests.Dapptest (dapptestTests)
import Tests.Encoding (encodingJSONTests)
import Tests.FoundryTestGen (foundryTestGenTests)
import Tests.Integration (integrationTests)
import Tests.Optimization (optimizationTests)
import Tests.PrefillCorpus (prefillCorpusTests)
import Tests.Overflow (overflowTests)
import Tests.Research (researchTests)
import Tests.Seed (seedTests)
import Tests.Symbolic (symbolicTests)
import Tests.Values (valuesTests)

main :: IO ()
main = withCurrentDirectory "./tests/solidity" . defaultMain $
         testGroup "Echidna"
           [ configTests
           , compilationTests
           , seedTests
           , integrationTests
           , valuesTests
           , coverageTests
           , abiv2Tests
           , assertionTests
           , overflowTests
           , optimizationTests
           , researchTests
           , dapptestTests
           , encodingJSONTests
           , foundryTestGenTests
           , prefillCorpusTests
           , cheatTests
           , symbolicTests
           ]
