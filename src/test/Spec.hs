{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty
import Test.Tasty.HUnit as HU

import Echidna.Campaign (Campaign(..), campaign, TestState(..))
import Echidna.Config (defaultConfig)
import Echidna.Solidity (contracts, loadSolidity)
import Echidna.Test (SolTest)
import Echidna.Transaction (World(..))

import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import EVM (state, contract)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Echidna" [solidityTests]

solidityTests :: TestTree
solidityTests = testGroup "Solidity-HUnit"
  [
    HU.testCase "Get Contracts" $ do
      c <- flip runReaderT defaultConfig $ contracts c1
      if length c == 3
      then return ()
      else assertFailure "Somehow we did not read 3 contracts"
  , HU.testCase "Old CLI" $ do
      testContract c2 $
        \(Campaign tests _) ->
          let findtest' = flip findtest tests in
          passed (fromJust (findtest' "echidna_alwaystrue")) &&
          solved (fromJust (findtest' "echidna_sometimesfalse"))
  ]
  where c1 = "./src/test/contracts/num-contracts.sol"
        c2 = "./src/test/contracts/cli.sol"

testContract :: FilePath -> (Campaign -> Bool) -> HU.Assertion
testContract file f = do
  results <- flip runReaderT defaultConfig $ do
    (v,a,ts) <- loadSolidity file Nothing
    let r = v ^. state . contract
    let w = World [0] [(r, a)]
    let ts' = zip ts (repeat r)
    campaign (pure ()) v w ts'
  if f results
  then return ()
  else assertFailure "Undesired campaign results found"

solved :: TestState -> Bool
solved (Large _ _) = True
solved (Solved _)  = True
solved _           = False

passed :: TestState -> Bool
passed Passed = True
passed _      = False

questionable :: TestState -> Bool
questionable = not . passed

findtest :: Text -> [(SolTest, TestState)] -> Maybe TestState
findtest _ [] = Nothing
findtest t ((st, ts):xs) = if t == fst st then Just ts else findtest t xs
