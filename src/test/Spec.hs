{-# LANGUAGE LambdaCase #-}

import Test.Tasty
import Test.Tasty.HUnit

import Echidna.ABI (SolCall, genInteractionsM, mkGenDict)
import Echidna.Campaign (Campaign(..), tests, campaign, TestState(..))
import Echidna.Config (defaultConfig, parseConfig, sConf)
import Echidna.Solidity
import Echidna.Transaction (Tx, call)

import Control.Lens
import Control.Monad (forM_, replicateM)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Maybe (isJust, maybe)
import Data.Text (Text, unpack)
import Data.List (find)
import EVM.ABI (AbiValue(..))
import System.Directory (withCurrentDirectory)

main :: IO ()
main = withCurrentDirectory "./examples/solidity" . defaultMain $
         testGroup "Echidna" [compilationTests, extractionTests, integrationTests]

-- Compilation Tests

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

-- Extraction Tests

extractionTests :: TestTree
extractionTests = testGroup "Constant extraction/generation testing"
  [ testCase "basic/constants.sol" . flip runReaderT (defaultConfig & sConf . quiet .~ True) $ do
      cs  <- contracts "basic/constants.sol"
      abi <- view _2 <$> loadSpecified Nothing cs
      is  <- evalStateT (replicateM 1000 $ genInteractionsM abi)
                      $ mkGenDict 0.15 (extractConstants cs) []
      forM_ [ ("ints",  ("find",  [AbiInt 256 1337]))
            , ("addrs", ("find2", [AbiAddress 0x123]))
            , ("strs",  ("find3", [AbiString "test"]))
            ] $ \(t, c) -> liftIO . assertBool ("failed to extract " ++ t) $ elem c is
  ]

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
  , testContract "basic/contractAddr.sol"        Nothing
      [ ("echidna_addr failed",                  not . solved "echidna_addr") ]
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
