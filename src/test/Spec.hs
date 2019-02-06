{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit as HU

import Echidna.Campaign (Campaign(..), tests, campaign, TestState(..))
import Echidna.Config (defaultConfig)
import Echidna.Solidity (contracts, loadSolidity)
import Echidna.Test (SolTest)
import Echidna.Transaction (World(..), Tx, call)

import Control.Lens ((^.))
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
      c <- flip runReaderT defaultConfig $ contracts c1
      assertBool "Somehow we did not read 3 contracts" $ length c == 3
  , HU.testCase "Old CLI" $ testContract c2 $
      \c -> do
        let findtest' = flip findtest (c ^. tests)
            t1 = fromJust $ findtest' "echidna_alwaystrue"
            t2 = fromJust $ findtest' "echidna_sometimesfalse"
        assertBool "echidna_alwaystrue did not pass" $ passed t1
        assertBool "echidna_sometimesfalse unsolved" $ solved t2
  , HU.testCase "Optimal Solve" $ testContract c3 $
      \c -> do
        let findtest' = flip findtest (c ^. tests)
            tr = fromJust $ findtest' "echidna_revert"
        assertBool "echidna_revert unsoved" $ solved tr
        let sol = solve tr
        assertBool "solution has length > 1" $ length sol == 1
        let sol' = head sol
        assertBool "solution is not f(-1)" $
          case sol' ^. call of
               Left ("f", [AbiInt _ (-1)]) -> True
               _                           -> False
  ]
  where c1 = "./src/test/contracts/num-contracts.sol"
        c2 = "./src/test/contracts/cli.sol"
        c3 = "./examples/solidity/basic/revert.sol"

testContract :: FilePath -> (Campaign -> HU.Assertion) -> HU.Assertion
testContract file f = do
  results <- flip runReaderT defaultConfig $ do
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

findtest :: Text -> [(SolTest, TestState)] -> Maybe TestState
findtest _ [] = Nothing
findtest t ((st, ts):xs) = if t == fst st then Just ts else findtest t xs
