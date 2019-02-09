{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import Test.Tasty
import Test.Tasty.HUnit as HU

import Echidna.Campaign (Campaign(..), tests, campaign, TestState(..))
import Echidna.Config (EConfig, defaultConfig, sConf, parseConfig)
import Echidna.Solidity (loadTesting, quiet)
import Echidna.Test (SolTest)
import Echidna.Transaction (Tx, call)

import Control.Lens ((^.), (&), (.~), view)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Data.Text (Text)
import EVM.ABI (AbiValue(..))

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "Echidna" [solidityTests]

solidityTests :: TestTree
solidityTests = testGroup "Solidity-HUnit"
  [ testCase "Always True" $ testContract' c5 $
      \c -> do
        let tr = findtest' c "echidna_true"
        assertBool "echidna_true somehow did not pass" $ passed tr
  , testCase "Simple Flags Example" $ testContract' c2 $
      \c -> do
        let t1 = findtest' c "echidna_alwaystrue"
            t2 = findtest' c "echidna_sometimesfalse"
        assertBool "echidna_alwaystrue did not pass" $ passed t1
        assertBool "echidna_sometimesfalse unsolved" $ solved t2
        let sol = solve t2
        assertBool "solution has length /= 2" $ length sol == 2
        -- the following check will fail with significantly higher probability
        -- due to its higher requirements of being met
        -- let calls@(call1:call2:[]) = view call <$> sol
        -- assertBool ("solution is not 'set0(0);set1(0);', was '" ++ show calls ++ "'") $
        --   case call1 of
        --        Left ("set0", [AbiInt _ 0]) -> True
        --        _                           -> False
        --   &&
        --   case call2 of
        --        Left ("set1", [AbiInt _ 0]) -> True
        --        _                           -> False
  , testCase "Optimal Solve" $ testContract' c3 $
      \c -> do
        let tr = findtest' c "echidna_revert"
        assertBool "echidna_revert unsolved" $ solved tr
        let sol = solve tr
        assertBool "solution has length /= 1" $ length sol == 1
        let sol' = head sol
        assertBool "solution is not f(-1)" $
          case sol' ^. call of
               Left ("f", [AbiInt _ (-1)]) -> True
               _                           -> False
  {-, testCase "Payment amounts" $ testContract c4 $
      \c -> do
        let tr = findtest' c "echidna_test"
        assertBool "echidna_test unsolved" $ solved tr -}
  , testCase "Multisender" $ testContract c6 (Right cfg6) $
      \c -> do
        let tr = findtest' c "echidna_all_sender"
        assertBool "echidna_all_sender unsolved" $ solved tr
        let sol = solve tr
        assertBool "solution has length /= 3" $ length sol == 3
        let calls = view call <$> sol
        assertBool "s1 not in solution" $ any (\case {Left ("s1", _) -> True; _ -> False;}) calls
        assertBool "s2 not in solution" $ any (\case {Left ("s2", _) -> True; _ -> False;}) calls
        assertBool "s3 not in solution" $ any (\case {Left ("s3", _) -> True; _ -> False;}) calls
  ]
  where c2   = "./examples/solidity/basic/flags.sol"
        c3   = "./examples/solidity/basic/revert.sol"
        --c4   = "./examples/solidity/basic/payable.sol"
        c5   = "./examples/solidity/basic/true.sol"
        c6   = "./examples/solidity/basic/multisender.sol"
        cfg6 = "./examples/solidity/basic/multisender.yaml"

testContract :: FilePath -> Either EConfig FilePath -> (Campaign -> HU.Assertion) -> HU.Assertion
testContract file cfg f = do
  cfg' <- case cfg of
               Left c   -> pure c
               Right fp -> maybe (pure defaultConfig) parseConfig (Just fp)
  let c = cfg' & sConf . quiet .~ True
  results <- flip runReaderT c $ do
    (v, w, ts) <- loadTesting file Nothing
    campaign (pure ()) v w ts
  f results

testContract' :: FilePath -> (Campaign -> HU.Assertion) -> HU.Assertion
testContract' = flip testContract (Left defaultConfig)

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
