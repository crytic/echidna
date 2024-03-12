import Test.Tasty (defaultMain, testGroup)
import System.Directory (withCurrentDirectory)
import Tests.ABIv2 (abiv2Tests)
import Tests.Assertion (assertionTests)
import Tests.Compile (compilationTests)
import Tests.Coverage (coverageTests)
import Tests.Config (configTests)
import Tests.Encoding (encodingJSONTests)
import Tests.Integration (integrationTests)
import Tests.Optimization (optimizationTests)
import Tests.Overflow (overflowTests)
import Tests.Research (researchTests)
import Tests.Values (valuesTests)
import Tests.Seed (seedTests)
import Tests.Dapptest (dapptestTests)
import Tests.Cheat (cheatTests)
import Tests.Symbolic (symbolicTests)

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
           , cheatTests
           , symbolicTests
           ]
