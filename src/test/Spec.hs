{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Test.Tasty
import Test.Tasty.HUnit as HU

import Echidna.Config (defaultConfig, returnType)
import Echidna.Exec (ePropertySeq, checkTest)
import Echidna.Solidity (readContracts, loadSolidity)

import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)

import Hedgehog as H
import Hedgehog.Internal.Property as HIP hiding (defaultConfig)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Echidna" [solidityTests]

solidityTests :: TestTree
solidityTests = testGroup "Solidity-HUnit"
  [
    HU.testCase "Get Contracts" $ do
      c <- flip runReaderT defaultConfig $ readContracts c1
      if length c == 3
      then return ()
      else assertFailure "Somehow we did not read 3 contracts"
  , HU.testCase "True Properties" $ do
      testContract c2 True
  , HU.testCase "False Property" $ do
      testContract c3 False
  , HU.testCase "Mixed Properties" $ do
      testContract c4 False
  ]
  where c1 = "./src/test/contracts/num-contracts.sol"
        c2 = "./src/test/contracts/cli_1.sol"
        c3 = "./src/test/contracts/cli_2.sol"
        c4 = "./src/test/contracts/cli_3.sol"

testContract :: FilePath -> Bool -> HU.Assertion
testContract file expected = do
  let f = checkTest (defaultConfig ^. returnType)
  x <- flip runReaderT defaultConfig $ do
    (v,a,ts) <- loadSolidity file Nothing
    let prop t = ePropertySeq (`f` t) a v >>= \x -> return (HIP.PropertyName $ show t, x)
    checkParallel . H.Group (HIP.GroupName file) =<< mapM prop ts
  if x == expected
  then return ()
  else assertFailure failureMessage
    where failureMessage = if expected then "Property failed" else "Property succeeded"
