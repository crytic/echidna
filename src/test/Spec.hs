{-# LANGUAGE LambdaCase #-}

import Test.Tasty
import Test.Tasty.HUnit

import Echidna.ABI (SolCall)
import Echidna.Campaign (Campaign(..), tests, campaign, TestState(..))
import Echidna.Config (defaultConfig, parseConfig, sConf)
import Echidna.Solidity (SolException(..), loadSolidity, loadSolTests, quiet)
import Echidna.Transaction (Tx, call)

import Control.Lens
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Reader (runReaderT)
import Data.Maybe (isJust, maybe)
import Data.Text (Text, unpack)
import Data.List (find)
import EVM.ABI (AbiValue(..))
import System.Directory (withCurrentDirectory)

main :: IO ()
main = withCurrentDirectory "./examples/solidity" . defaultMain $
         testGroup "Echidna" [compilationTests, integrationTests]

-- Compilation tests

compilationTests :: TestTree
compilationTests = testGroup "Compilation and loading tests"
  [ loadFails "bad/nocontract.sol" (Just "c") "failed to warn on contract not found" $
                                              \case ContractNotFound{} -> True; _ -> False
  , loadFails "bad/nobytecode.sol" Nothing    "failed to warn on abstract contract" $
                                              \case NoBytecode{}       -> True; _ -> False
  , loadFails "bad/nofuncs.sol"    Nothing    "failed to warn on no functions found" $
                                              \case NoFuncs{}          -> True; _ -> False
  , loadFails "bad/notests.sol"    Nothing    "failed to warn on no tests found" $
                                              \case NoTests{}          -> True; _ -> False
  , loadFails "bad/onlytests.sol"  Nothing    "failed to warn on no non-tests found" $
                                              \case OnlyTests{}        -> True; _ -> False
  , loadFails "bad/testargs.sol"   Nothing    "failed to warn on test args found" $
                                              \case TestArgsFound{}    -> True; _ -> False
  ]

loadFails :: FilePath -> Maybe Text -> String -> (SolException -> Bool) -> TestTree
loadFails fp c e p = testCase fp . catch tryLoad $ assertBool e . p where
  tryLoad = runReaderT (loadSolidity fp c >> pure ()) $ defaultConfig & sConf . quiet .~ True

-- Integration Tests

integrationTests :: TestTree
integrationTests = testGroup "Solidity Integration Testing"
  [ testContract "basic/true.sol"        Nothing
      [ ("echidna_true failed",                            not . solved "echidna_true") ]
  , testContract "basic/flags.sol"       Nothing
      [ ("echidna_alwaystrue failed",                      not . solved "echidna_alwaystrue")
      , ("echidna_sometimesfalse passed",                  solved       "echidna_sometimesfalse")
      , ("echidna_sometimesfalse didn't shrink optimally", solvedLen 2  "echidna_sometimesfalse")
      ]
  , testContract "basic/revert.sol"      Nothing
      [ ("echidna_revert passed",                          solved      "echidna_revert")
      , ("echidna_revert didn't shrink to length 1",       solvedLen 1 "echidna_revert")
      , ("echidna_revert didn't shrink to f(-1)",
         solvedWith ("f", [AbiInt 256 (-1)]) "echidna_revert")
      ]
  , testContract "basic/multisender.sol" (Just "basic/multisender.yaml") $
      [ ("echidna_all_sender passed",                      solved             "echidna_all_sender")
      , ("echidna_all_sender didn't shrink optimally",     solvedLen 3        "echidna_all_sender")
      ] ++ (["s1", "s2", "s3"] <&> \n ->
        ("echidna_all_sender solved without " ++ unpack n, solvedWith (n, []) "echidna_all_sender"))
  , testContract "basic/constants.sol"   Nothing
      [ ("echidna_found failed (didn't find constant)",    solved "echidna_found") ]
  ]

testContract :: FilePath -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContract fp cfg as = testCase fp $ do
  c <- set (sConf . quiet) True <$> maybe (pure defaultConfig) parseConfig cfg
  res <- runReaderT (loadSolTests fp Nothing >>= \(v, w, ts) -> campaign (pure ()) v w ts Nothing) c
  mapM_ (\(t,f) -> assertBool t $ f res) as

solnFor :: Text -> Campaign -> Maybe [Tx]
solnFor t c = case fmap snd <$> find ((t ==) . fst . fst) $ c ^. tests of
  Just (Large _ s) -> Just s
  Just (Solved  s) -> Just s
  _                -> Nothing

solved :: Text -> Campaign -> Bool
solved t = isJust . solnFor t

solvedLen :: Int -> Text -> Campaign -> Bool
solvedLen i t = (== Just i) . fmap length . solnFor t

-- NOTE: this just verifies a call was found in the solution. Doesn't care about ordering/seq length
solvedWith :: SolCall -> Text -> Campaign -> Bool
solvedWith c t = maybe False (any $ (== Left c) . view call) . solnFor t
