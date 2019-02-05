import Test.Tasty
import Test.Tasty.HUnit as HU

import Echidna.Config (defaultConfig)
import Echidna.Solidity (readContracts)

import Control.Monad.Reader (runReaderT)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Echidna" [solidityTests]

solidityTests :: TestTree
solidityTests = testGroup "Solidity-HUnit"
  [
    HU.testCase "Get Contracts" $ do
      c <- flip runReaderT defaultConfig $ readContracts c1
      if length c == 3 then
        return ()
      else
        assertFailure "Somehow we did not read 3 contracts"
  ]
  where c1 = "./src/test/contracts/num-contracts.sol"
