{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck(Arbitrary(..), Gen, (===), property, testProperty, resize)

import EVM (env, contracts)
import EVM.ABI (AbiValue(..))
import EVM.Types (Addr)
import qualified EVM.Concrete(Word(..))

import Echidna.ABI (SolCall, mkGenDict)
import Echidna.Campaign (Campaign(..), CampaignConf(..), TestState(..), campaign, tests, gasInfo)
import Echidna.Config (EConfig, EConfigWithUsage(..), _econfig, defaultConfig, parseConfig, sConf, cConf)
import Echidna.Solidity
import Echidna.Transaction (TxCall(..), Tx(..), call)

import Data.Aeson (encode, decode)
import Control.Lens
import Control.Monad (liftM2, void, when)
import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.Random (getRandom)
import Control.Monad.Reader (runReaderT)
import Data.Map (lookup)
import Data.Map.Strict (keys)
import Data.Maybe (isJust, maybe)
import Data.Text (Text, unpack, pack)
import Data.List (find, isInfixOf)

import System.Directory (withCurrentDirectory)
import System.Process (readProcess)

import qualified Data.List.NonEmpty   as NE

main :: IO ()
main = withCurrentDirectory "./examples/solidity" . defaultMain $
         testGroup "Echidna" [ configTests
                             , compilationTests
                             , seedTests
                             , integrationTests
                             , researchTests
                             , encodingJSONTests
                             ]

-- Configuration Tests

configTests :: TestTree
configTests = testGroup "Configuration tests" $
  [ testCase file $ void $ parseConfig file | file <- files ] ++
  [ testCase "parse \"coverage: true\"" $ do
      config <- _econfig <$> parseConfig "coverage/test.yaml"
      assertCoverage config $ Just mempty
  , testCase "coverage disabled by default" $
      assertCoverage defaultConfig Nothing
  , testCase "default.yaml" $ do
      EConfigWithUsage _ bad unset <- parseConfig "basic/default.yaml"
      assertBool ("unused options: " ++ show bad) $ null bad
      let unset' = unset & sans "seed"
      assertBool ("unset options: " ++ show unset') $ null unset'
  ]
  where files = ["basic/config.yaml", "basic/default.yaml"]
        assertCoverage config value = do
          let CampaignConf{knownCoverage} = view cConf config
          knownCoverage @?= value

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
  , loadFails "bad/consargs.sol"   Nothing    "failed to warn on cons args found" $
                                              \case ConstructorArgs{}  -> True; _ -> False
  ]

loadFails :: FilePath -> Maybe Text -> String -> (SolException -> Bool) -> TestTree
loadFails fp c e p = testCase fp . catch tryLoad $ assertBool e . p where
  tryLoad = runReaderT (loadWithCryticCompile (fp NE.:| []) c >> pure ()) testConfig

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
                                & cConf .~ CampaignConf 600 False False 20 0 Nothing (Just s) 0.15 Nothing
          gen s = view tests <$> runContract "basic/flags.sol" Nothing (cfg s)
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
  , testContract "basic/flags.sol" (Just "basic/whitelist.yaml")
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  passed      "echidna_sometimesfalse")
      ]
  , testContract "basic/flags.sol" (Just "basic/whitelist_all.yaml")
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  solved      "echidna_sometimesfalse")
      ]
  , testContract "basic/flags.sol" (Just "basic/blacklist.yaml")
      [ ("echidna_alwaystrue failed",                      passed      "echidna_alwaystrue")
      , ("echidna_revert_always failed",                   passed      "echidna_revert_always")
      , ("echidna_sometimesfalse passed",                  passed      "echidna_sometimesfalse")
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

  , testContract' "basic/smallValues.sol" Nothing (Just "coverage/test.yaml") False Nothing
      [ ("echidna_findSmall passed", solved "echidna_findSmall") ]

  , testContract "basic/multisender.sol" (Just "basic/multisender.yaml") $
      [ ("echidna_all_sender passed",                      solved             "echidna_all_sender")
      , ("echidna_all_sender didn't shrink optimally",     solvedLen 3        "echidna_all_sender")
      ] ++ (["s1", "s2", "s3"] <&> \n ->
        ("echidna_all_sender solved without " ++ unpack n, solvedWith (n, []) "echidna_all_sender"))

  , testContract "basic/memory-reset.sol" Nothing
      [ ("echidna_memory failed",                  passed      "echidna_memory") ]
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
  , testContract' "basic/cons-create-2.sol" (Just "C") Nothing True Nothing
      [ ("echidna_state failed",                   solved      "echidna_state") ]
-- single.sol is really slow and kind of unstable. it also messes up travis.
--  , testContract "coverage/single.sol"    (Just "coverage/test.yaml")
--      [ ("echidna_state failed",                   solved      "echidna_state") ]
  , testContract "coverage/multi.sol"     Nothing
      [ ("echidna_state3 failed",                  solved      "echidna_state3") ]
  , testContract "basic/balance.sol"      (Just "basic/balance.yaml")
      [ ("echidna_balance failed",                 passed      "echidna_balance") ]
  , testContract "basic/library.sol"      (Just "basic/library.yaml")
      [ ("echidna_library_call failed",            solved      "echidna_library_call") ]
  , testContract' "basic/fallback.sol" Nothing Nothing True (Just ["0.4.25", "0.5.7"])
      [ ("echidna_fallback failed",                solved      "echidna_fallback") ]
  , testContract "basic/darray.sol"       Nothing
      [ ("echidna_darray passed",                  solved      "echidna_darray")
      , ("echidna_darray didn't shrink optimally", solvedLen 1 "echidna_darray") ]
  , testContract "basic/propGasLimit.sol" (Just "basic/propGasLimit.yaml")
      [ ("echidna_runForever passed",              solved      "echidna_runForever") ]
  , testContract "basic/assert.sol"       (Just "basic/assert.yaml")
      [ ("echidna_set0 passed",                    solved      "ASSERTION set0")
      , ("echidna_set1 failed",                    passed      "ASSERTION set1") ]
  , testContract "basic/time.sol"         (Just "basic/time.yaml")
      [ ("echidna_timepassed passed",              solved      "echidna_timepassed") ]
  , testContract "basic/construct.sol"    Nothing
      [ ("echidna_construct passed",               solved      "echidna_construct") ]
  , testContract "basic/gasprice.sol"     Nothing
      [ ("echidna_state passed",                   solved      "echidna_state") ]
  , testContract' "basic_multicontract/contracts/Foo.sol" Nothing (Just "basic_multicontract/echidna_config.yaml") True (Just ["0.4.25"])
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract' "basic/multi-abi.sol" (Just "B") (Just "basic/multi-abi.yaml") True Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/Ballot.sol"       Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/Dynamic.sol"      Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/Dynamic2.sol"     Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "abiv2/MultiTuple.sol"   Nothing
      [ ("echidna_test passed",                    solved      "echidna_test") ]
  , testContract "basic/gasuse.sol"       (Just "basic/gasuse.yaml")
      [ ("echidna_true failed",                    passed     "echidna_true")
      , ("g gas estimate wrong",                   gasInRange "g" 15000000 40000000)
      , ("f_close1 gas estimate wrong",            gasInRange "f_close1" 1800 2000)
      , ("f_open1 gas estimate wrong",             gasInRange "f_open1"  18000 23000)
      , ("push_b gas estimate wrong",              gasInRange "push_b"   39000 45000)
      ]
  ]

researchTests :: TestTree
researchTests = testGroup "Research-based Integration Testing"
  [ testContract "research/harvey_foo.sol" Nothing
      [ ("echidna_assert failed",                  solved      "echidna_assert") ]
  , testContract "research/harvey_baz.sol" Nothing
      [ ("echidna_all_states failed",              solved      "echidna_all_states") ]
  ]

testConfig :: EConfig
testConfig = defaultConfig & sConf . quiet .~ True
                           & cConf .~ (defaultConfig ^. cConf) { testLimit = 10000, shrinkLimit = 2500 }

testContract :: FilePath -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContract fp cfg =
  testContract' fp Nothing cfg True Nothing

testContract' :: FilePath -> Maybe Text -> Maybe FilePath -> Bool -> Maybe [String] -> [(String, Campaign -> Bool)] -> TestTree
testContract' fp n cfg s vs as = testCase fp $
  case vs of
       Just vs' -> do
         sv <- readProcess "solc" ["--version"] ""
         when (any (`isInfixOf` sv) (("Version: "<>) <$> vs')) doTest
       Nothing  -> doTest
  where doTest = do
          c <- set (sConf . quiet) True <$> maybe (pure testConfig) (fmap _econfig . parseConfig) cfg
          let c' = c & sConf . quiet .~ True
                     & if s then cConf .~ (c ^. cConf) { testLimit = 10000, shrinkLimit = 2500 } else id
          res <- runContract fp n c'
          mapM_ (\(t,f) -> assertBool t $ f res) as

runContract :: FilePath -> Maybe Text -> EConfig -> IO Campaign
runContract fp n c =
  flip runReaderT c $ do
    g <- getRandom
    (v,w,ts) <- loadSolTests (fp NE.:| []) n
    cs  <- Echidna.Solidity.contracts (fp NE.:| [])
    ads <- NE.toList <$> addresses
    let ads' = AbiAddress <$> v ^. env . EVM.contracts . to keys
    campaign (pure ()) v w ts (Just $ mkGenDict 0.15 (extractConstants cs ++ ads ++ ads') [] g (returnTypes cs)) []

getResult :: Text -> Campaign -> Maybe TestState
getResult t = fmap snd <$> find ((t ==) . either fst (("ASSERTION " <>) . fst) . fst) . view tests

getGas :: Text -> Campaign -> Maybe (Int, [Tx])
getGas t = Data.Map.lookup t . view gasInfo

gasInRange :: Text -> Int -> Int -> Campaign -> Bool
gasInRange t l h c = case getGas t c of
  Just (g, _) -> g >= l && g <= h
  _           -> False

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
solvedWith c t = maybe False (any $ (== SolCall c) . view call) . solnFor t

-- Encoding JSON tests

instance Arbitrary Addr where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary EVM.Concrete.Word where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary TxCall where
  arbitrary = do
                s <- arbitrary
                cs <- resize 32 arbitrary
                return $ SolCall (pack s, cs)

instance Arbitrary Tx where
  arbitrary = let a :: Arbitrary a => Gen a
                  a = arbitrary in
                Tx <$> a <*> a <*> a <*> a <*> a <*> a <*> a

encodingJSONTests :: TestTree
encodingJSONTests =
  testGroup "Tx JSON encoding"
    [ testProperty "decode . encode = id" $ property $ do
        t <- arbitrary :: Gen Tx
        return $ decode (encode t) === Just t
    ]
