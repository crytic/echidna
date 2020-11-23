module Common
  ( testConfig
  , runContract
  , testContract
  , testContract'
  , checkConstructorConditions
  , solnFor
  , solved
  , passed
  , solvedLen
  , solvedWith
  , getGas
  , gasInRange
  , countCorpus
  , testsEmpty
  , coverageEmpty
  ) where

import Prelude hiding (lookup)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertBool)

import Control.Lens (view, set, (.~), (^.))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Control.Monad.State.Strict (evalStateT)
import Data.Function ((&))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (lookup, empty)
import Data.Maybe (isJust)
import Data.Text (Text)
import Echidna (prepareContract)
import Echidna.Campaign (campaign)
import Echidna.Config (EConfig, _econfig, parseConfig, defaultConfig, sConf, cConf)
import Echidna.Solidity (loadSolTests, quiet)
import Echidna.Test (checkETest)
import Echidna.Types.Campaign (Campaign, TestState(..), testLimit, shrinkLimit, tests, gasInfo, corpus, coverage)
import Echidna.Types.Signature (SolCall)
import Echidna.Types.Tx (Tx(..), TxCall(..), call)
import Echidna.Types.World (eventMap)

testConfig :: EConfig
testConfig = defaultConfig & sConf . quiet .~ True
                           & cConf . testLimit .~ 10000
                           & cConf . shrinkLimit .~ 4000

runContract :: FilePath -> Maybe String -> EConfig -> IO Campaign
runContract f c cfg =
  flip runReaderT cfg $ do
    g <- getRandom
    (v, _, _, w, ts, d, txs) <- prepareContract cfg (f :| []) c g
    -- start ui and run tests
    campaign (pure ()) v w ts d txs

testContract :: FilePath -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContract fp cfg = testContract' fp Nothing cfg True

testContract' :: FilePath -> Maybe String -> Maybe FilePath -> Bool -> [(String, Campaign -> Bool)] -> TestTree
testContract' fp n cfg s as = testCase fp $ do
  c <- set (sConf . quiet) True <$> maybe (pure testConfig) (fmap _econfig . parseConfig) cfg
  let c' = c & sConf . quiet .~ True
             & (if s then cConf . testLimit .~ 10000 else id)
             & (if s then cConf . shrinkLimit .~ 4000 else id)
  res <- runContract fp n c'
  mapM_ (\(t,f) -> assertBool t $ f res) as

checkConstructorConditions :: FilePath -> String -> TestTree
checkConstructorConditions fp as = testCase fp $ do
  r <- flip runReaderT testConfig $ do
    (v, w, t) <- loadSolTests (fp :| []) Nothing
    let em = w ^. eventMap
    mapM (\u -> evalStateT (checkETest em u) v) t
  mapM_ (assertBool as) r

getResult :: Text -> Campaign -> Maybe TestState
getResult t = fmap snd <$> find ((t ==) . either fst (("ASSERTION " <>) . fst) . fst) . view tests

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

getGas :: Text -> Campaign -> Maybe (Int, [Tx])
getGas t = lookup t . view gasInfo

gasInRange :: Text -> Int -> Int -> Campaign -> Bool
gasInRange t l h c = case getGas t c of
  Just (g, _) -> g >= l && g <= h
  _           -> False

countCorpus :: Int -> Campaign -> Bool
countCorpus n c = length (view corpus c) == n

testsEmpty :: Campaign -> Bool
testsEmpty c = null (view tests c)

coverageEmpty :: Campaign -> Bool
coverageEmpty c = view coverage c == empty
