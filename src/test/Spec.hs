{-# LANGUAGE LambdaCase #-}

import Test.Tasty
import Test.Tasty.HUnit

import Echidna.ABI (SolCall, mkGenDict)
import Echidna.Campaign (Campaign(..), CampaignConf(..), TestState(..), campaign, tests)
import Echidna.Config (EConfig, defaultConfig, parseConfig, sConf, cConf)
import Echidna.Solidity
import Echidna.Transaction (Tx, call)

import Control.Lens
import Control.Monad (liftM2)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Random (getRandom)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (isJust, maybe)
import Data.Text (Text, unpack)
import Data.List (find)
import EVM.ABI (AbiValue(..))
import System.Directory (withCurrentDirectory)

main :: IO ()
main = withCurrentDirectory "./examples/solidity" . defaultMain $
         testGroup "Echidna" [compilationTests, seedTests, integrationTests]

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
  tryLoad = runReaderT (loadWithCryticCompile fp c >> pure ()) $ defaultConfig & sConf . quiet .~ True

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


seedTests :: TestTree
seedTests =
  testGroup "Seed reproducibility testing"
    [ testCase "different seeds" $ assertBool "results are the same" . not =<< same 0 1
    , testCase "same seeds" $ assertBool "results differ" =<< same 0 0
    ]
    where cfg s = defaultConfig & sConf . quiet .~ True
                                & cConf .~ CampaignConf 600 5 0 Nothing (Just s)
          gen s = view tests <$> runContract "basic/flags.sol" (cfg s)
          same s t = liftM2 (==) (gen s) (gen t)

-- Integration Tests

integrationTests :: TestTree
integrationTests = testGroup "Solidity Integration Testing"
  [ testContract "basic/true.sol" Nothing
      [ ("echidna_true failed", passed "echidna_true") ]
  , testContract "basic/flags.sol" Nothing
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  solved      "echidna_sometimesfalse")
      , ("echidna_sometimesfalse didn't shrink optimally", solvedLen 2 "echidna_sometimesfalse")
      ]
  , testContract "basic/revert.sol" Nothing
      [ ("echidna_fails_on_revert passed", solved "echidna_fails_on_revert")
      , ("echidna_fails_on_revert didn't shrink to one transaction",
         solvedLen 1 "echidna_fails_on_revert")
      , ("echidna_revert_is_false didn't shrink to f(-1)",
         solvedWith ("f", [AbiInt 256 (-1)]) "echidna_fails_on_revert")
      ]
  
  , testContract "basic/nearbyMining.sol" (Just "coverage/test.yaml")
      [ ("echidna_findNearby passed", solved "echidna_findNearby") ]

  , testContract "basic/smallValues.sol" (Just "coverage/test.yaml")
      [ ("echidna_findSmall passed", solved "echidna_findSmall") ]

  , testContract "basic/multisender.sol" (Just "basic/multisender.yaml") $
      [ ("echidna_all_sender passed",                      solved             "echidna_all_sender")
      , ("echidna_all_sender didn't shrink optimally",     solvedLen 3        "echidna_all_sender")
      ] ++ (["s1", "s2", "s3"] <&> \n ->
        ("echidna_all_sender solved without " ++ unpack n, solvedWith (n, []) "echidna_all_sender"))

  , testContract "basic/memory-reset.sol" Nothing
      [ ("echidna_memory failed",      passed "echidna_memory") ]
  , testContract "basic/contractAddr.sol" Nothing
      [ ("echidna_address failed",                 solved      "echidna_address") ]
  , testContract "basic/contractAddr.sol" (Just "basic/contractAddr.yaml")
      [ ("echidna_address failed",                 passed      "echidna_address") ]      
  , testContract "basic/constants.sol"    Nothing
      [ ("echidna_found failed",                   solved      "echidna_found") ]
  , testContract "basic/constants2.sol"   Nothing
      [ ("echidna_found32 failed",                 solved      "echidna_found32") ]
  , testContract "basic/constants3.sol"   Nothing
      [ ("echidna_found_sender failed",            solved      "echidna_found_sender") ] 
  , testContract "basic/rconstants.sol"   Nothing
      [ ("echidna_found failed",                   solved      "echidna_found") ]
-- single.sol is really slow and kind of unstable. it also messes up travis.
--  , testContract "coverage/single.sol"    (Just "coverage/test.yaml")
--      [ ("echidna_state failed",                   solved      "echidna_state") ]
  , testContract "coverage/multi.sol"     Nothing
      [ ("echidna_state3 failed",                  solved      "echidna_state3") ]
  , testContract "basic/balance.sol"      (Just "basic/balance.yaml")
      [ ("echidna_balance failed",                 passed      "echidna_balance") ]
  , testContract "basic/library.sol"      (Just "basic/library.yaml")
      [ ("echidna_library_call failed",            solved "echidna_library_call") ]
  , testContract "harvey/foo.sol"         Nothing
      [ ("echidna_assert failed",                  solved "echidna_assert") ]
  , testContract "harvey/baz.sol"         Nothing
      [ ("echidna_all_states failed",              solved "echidna_all_states") ]
  , testContract "basic/fallback.sol"     Nothing
      [ ("echidna_fallback failed",                solved      "echidna_fallback") ]
  , testContract "basic/darray.sol"       Nothing
      [ ("echidna_darray passed",                  solved      "echidna_darray")
      , ("echidna_darray didn't shrink optimally", solvedLen 1 "echidna_darray") ]
  , testContract "basic/propGasLimit.sol" (Just "basic/propGasLimit.yaml") 
      [ ("echidna_runForever passed",              solved      "echidna_runForever")]
  ]

testContract :: FilePath -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContract fp cfg as = testCase fp $ do
  c <- set (sConf . quiet) True <$> maybe (pure defaultConfig) parseConfig cfg
  res <- runContract fp c
  mapM_ (\(t,f) -> assertBool t $ f res) as

runContract :: FilePath -> EConfig -> IO Campaign
runContract fp c =
  flip runReaderT c $ do
    g <- getRandom
    (v,w,ts) <- loadSolTests fp Nothing
    cs  <- contracts fp
    ads <- addresses
    campaign (pure ()) v w ts (Just $ mkGenDict 0.15 (extractConstants cs ++ ads) [] g (returnTypes cs))

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
