{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit as HU

import Echidna.Campaign (Campaign(..), tests, campaign, TestState(..))
import Echidna.Config (EConfig, defaultConfig, sConf)
import Echidna.Solidity (contracts, loadSolidity, quiet)
import Echidna.Test (SolTest)
import Echidna.Transaction (World(..), Tx, call)

import Control.Lens ((^.), (&), (.~))
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import EVM (state, contract)
import EVM.ABI (AbiValue(..))

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "Echidna" [solidityTests]

solidityTests :: TestTree
solidityTests = testGroup "Solidity-HUnit"
  [
    HU.testCase "Get Contracts" $ do
      c <- flip runReaderT testConfig $ contracts c1
      assertBool "Somehow we did not read 3 contracts" $ length c == 3
  , HU.testCase "Always True" $ testContract c5 $
      \c -> do
        let tr = findtest' c "echidna_true"
        assertBool "echidna_true somehow did not pass" $ passed tr
  , HU.testCase "Old CLI" $ testContract c2 $
      \c -> do
        let t1 = findtest' c "echidna_alwaystrue"
            t2 = findtest' c "echidna_sometimesfalse"
        assertBool "echidna_alwaystrue did not pass" $ passed t1
        assertBool "echidna_sometimesfalse unsolved" $ solved t2
  , HU.testCase "Optimal Solve" $ testContract c3 $
      \c -> do
        let tr = findtest' c "echidna_revert"
        assertBool "echidna_revert unsolved" $ solved tr
        let sol = solve tr
        assertBool "solution has length > 1" $ length sol == 1
        let sol' = head sol
        assertBool "solution is not f(-1)" $
          case sol' ^. call of
               Left ("f", [AbiInt _ (-1)]) -> True
               _                           -> False
  , HU.testCase "Payment amounts" $ testContract c4 $
      \c -> do
        let tr = findtest' c "echidna_test"
        assertBool "echidna_test unsolved" $ solved tr
  ]
  where c1 = "./src/test/contracts/num-contracts.sol"
        c2 = "./src/test/contracts/cli.sol"
        c3 = "./examples/solidity/basic/revert.sol"
        c4 = "./examples/solidity/basic/payable.sol"
        c5 = "./src/test/contracts/true.sol"

testContract :: FilePath -> (Campaign -> HU.Assertion) -> HU.Assertion
testContract file f = do
  results <- flip runReaderT testConfig $ do
    (v,a,ts) <- loadSolidity file Nothing
    let r = v ^. state . contract
        w = World [0] [(r, a)]
        ts' = zip ts (repeat r)
    campaign (pure ()) v w ts'
  f results

solved :: TestState -> Bool
solved (Large _ _) = True
solved (Solved _)  = True
solved _           = False

passed :: TestState -> Bool
passed Passed = True
passed _      = False

solve :: TestState -> [Tx]
solve (Large _ s) = s
solve (Solved s) = s
solve _ = undefined

findtest :: Campaign -> Text -> Maybe TestState
findtest = findtest'' . (^. tests)

findtest' :: Campaign -> Text -> TestState
findtest' = (fromJust .) . findtest

findtest'' :: [(SolTest, TestState)] -> Text -> Maybe TestState
findtest'' [] _ = Nothing
findtest'' ((st, ts):xs) t = if t == fst st then Just ts else findtest'' xs t

testConfig :: EConfig
testConfig = defaultConfig & sConf . quiet .~ True
