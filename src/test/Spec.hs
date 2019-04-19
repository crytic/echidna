{-# LANGUAGE LambdaCase #-}

import Test.Tasty
import Test.Tasty.HUnit

import Echidna.ABI (SolCall, mkGenDict)
import Echidna.Campaign (Campaign(..), tests, campaign, TestState(..))
import Echidna.Config (defaultConfig, parseConfig, sConf)
import Echidna.Solidity
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
         testGroup "Echidna" [compilationTests, {- extractionTests,-} integrationTests]

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


-- We need to rethink this test
{-
extractionTests :: TestTree
extractionTests = testGroup "Constant extraction/generation testing"
  [ testCase "basic/constants.sol" . flip runReaderT (defaultConfig & sConf . quiet .~ True) $ do
      cs  <- contracts "basic/constants.sol"
      abi <- view _2 <$> loadSpecified Nothing cs
      is  <- evalStateT (replicateM 1000 $ genInteractionsM abi)
                      $ mkGenDict 0.15 (extractConstants cs) []
      forM_ [ ("ints",  ("find",  [AbiInt 256 1447]))
            ("addrs", ("find2", [AbiAddress 0x123]))
            , ("strs",  ("find3", [AbiString "test"]))
            ] $ \(t, c) -> liftIO . assertBool ("failed to extract " ++ t ++ " " ++ show (c,is)) $ elem c is
  ]
-}

-- Integration Tests

integrationTests :: TestTree
integrationTests = testGroup "Solidity Integration Testing"
  [ testContract "basic/true.sol"        Nothing
      [ ("echidna_true failed",                            passed       "echidna_true") ]
  , testContract "basic/flags.sol"       Nothing
      [ ("echidna_alwaystrue failed",                      passed       "echidna_alwaystrue")
      , ("echidna_sometimesfalse passed",                  solved       "echidna_sometimesfalse")
      , ("echidna_sometimesfalse didn't shrink optimally", solvedLen 2  "echidna_sometimesfalse")
      ]
  , testContract "basic/revert.sol"      Nothing
      [ ("echidna_revert passed",                          solved      "echidna_revert")
      , ("echidna_revert didn't shrink to length 1",       solvedLen 1 "echidna_revert")
      , ("echidna_revert didn't shrink to f(-1)",
         solvedWith ("f", [AbiInt 256 (-1)]) "echidna_revert")
      ]
  
  , testContract "basic/nearbyMining.sol"     (Just "coverage/test.yaml")
      [ ("echidna_findNearby passed",                solved       "echidna_findNearby") ]

  , testContract "basic/smallValues.sol"      (Just "coverage/test.yaml")
      [ ("echidna_findSmall passed",                solved       "echidna_findSmall") ]

  , testContract "basic/multisender.sol" (Just "basic/multisender.yaml") $
      [ ("echidna_all_sender passed",                      solved             "echidna_all_sender")
      , ("echidna_all_sender didn't shrink optimally",     solvedLen 3        "echidna_all_sender")
      ] ++ (["s1", "s2", "s3"] <&> \n ->
        ("echidna_all_sender solved without " ++ unpack n, solvedWith (n, []) "echidna_all_sender"))
        
  , testContract "basic/propGasLimit.sol" (Just "basic/propGasLimit.yaml") 
      [ ("echidna_runForever passed",             solved "echidna_runForever") ]
  , testContract "basic/contractAddr.sol" Nothing
      [ ("echidna_address failed",                solved "echidna_address") ]
  , testContract "basic/contractAddr.sol" (Just "basic/contractAddr.yaml")
      [ ("echidna_address failed",                passed "echidna_address") ]      
  , testContract "basic/constants.sol"    Nothing
      [ ("echidna_found failed",                  solved "echidna_found") ]
  , testContract "basic/constants2.sol"   Nothing
      [ ("echidna_found32 failed",                solved "echidna_found32") ]
  , testContract "coverage/single.sol"    (Just "coverage/test.yaml")
      [ ("echidna_state failed",                  solved "echidna_state") ]
  , testContract "coverage/multi.sol"     Nothing
      [ ("echidna_state3 failed",                 solved "echidna_state3") ]
  , testContract "basic/balance.sol"      (Just "basic/balance.yaml")
      [ ("echidna_balance failed",                passed "echidna_balance") ]

  ]

testContract :: FilePath -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContract fp cfg as = testCase fp $ do
  c <- set (sConf . quiet) True <$> maybe (pure defaultConfig) parseConfig cfg
  res <- flip runReaderT c $ do
           (v,w,ts) <- loadSolTests fp Nothing
           cs  <- contracts fp
           campaign (pure ()) v w ts (Just $ mkGenDict 0.15 (extractConstants cs) [])
  mapM_ (\(t,f) -> assertBool t $ f res) as

getResult :: Text -> Campaign -> Maybe TestState
getResult t = fmap snd <$> find ((t ==) . fst . fst) . view tests

solnFor :: Text -> Campaign -> Maybe [Tx]
solnFor t c = case getResult t c of
  Just (Large _ s) -> Just s
  Just (Solved  s) -> Just s
  _                -> Nothing

solved :: Text -> Campaign -> Bool
solved t = isJust . solnFor t

passed :: Text -> Campaign -> Bool
passed t c = case getResult t c of
  Just (Open _) -> True
  Just Passed   -> True
  _             -> False

solvedLen :: Int -> Text -> Campaign -> Bool
solvedLen i t = (== Just i) . fmap length . solnFor t

-- NOTE: this just verifies a call was found in the solution. Doesn't care about ordering/seq length
solvedWith :: SolCall -> Text -> Campaign -> Bool
solvedWith c t = maybe False (any $ (== Left c) . view call) . solnFor t
