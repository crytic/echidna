module Common
  ( testConfig
  , runContract
  , testContract
  , testContractV
  , solcV
  , testContract'
  , checkConstructorConditions
  , optimized
  , solnFor
  , solved
  , passed
  , solvedLen
  , solvedWith
  , solvedWithout
  , solvedUsing
  , getGas
  , gasInRange
  , countCorpus
  , coverageEmpty
  , overrideQuiet
  ) where

import Prelude hiding (lookup)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertBool)

import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandom)
import Control.Monad.State.Strict (evalStateT)
import Data.DoubleWord (Int256)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (splitOn)
import Data.Map (fromList, lookup, empty)
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.SemVer (Version, version, fromText)
import System.Process (readProcess)

import Echidna (prepareContract)
import Echidna.Config (parseConfig, defaultConfig)
import Echidna.Campaign (campaign)
import Echidna.Solidity (loadSolTests)
import Echidna.Test (checkETest)
import Echidna.Types.Config (Env(..), EConfig(..), EConfigWithUsage(..))
import Echidna.Types.Campaign (Campaign(..), CampaignConf(..))
import Echidna.Types.Signature (ContractName)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test
import Echidna.Types.Tx (Tx(..), TxCall(..), call)

import EVM.Dapp (dappInfo, emptyDapp)
import EVM.Solidity (SolcContract(..))

testConfig :: EConfig
testConfig = defaultConfig & overrideQuiet
                           & overrideLimits

overrideQuiet :: EConfig -> EConfig
overrideQuiet conf =
  conf { solConf = conf.solConf { quiet = True } }

overrideLimits :: EConfig -> EConfig
overrideLimits conf =
  conf { campaignConf = conf.campaignConf { testLimit = 10000
                                          , shrinkLimit = 4000
                                          }
       }

type SolcVersion = Version
type SolcVersionComp = Version -> Bool

solcV :: (Int, Int, Int) -> SolcVersion
solcV (x,y,z) = version x y z [] []

withSolcVersion :: Maybe SolcVersionComp -> IO () -> IO ()
withSolcVersion Nothing t = t
withSolcVersion (Just f) t = do
  sv <- readProcess "solc" ["--version"] ""
  let sv' = case splitOn "Version: " sv of
              _:x:_ -> x
              _ -> error "unexpected output"
  let sv'' = case splitOn "+" sv' of
               x:_ -> x
               _ -> error "unexpected output"
  case fromText $ pack sv'' of
    Right v' -> if f v' then t else assertBool "skip" True
    Left e   -> error $ show e

runContract :: FilePath -> Maybe ContractName -> EConfig -> IO Campaign
runContract f mc cfg = do
  g <- getRandom
  (v, sc, cs, w, ts, d, txs) <- prepareContract cfg (f :| []) mc g
  let solcByName = fromList [(c.contractName, c) | c <- cs]
  let dappInfo' = dappInfo "/" solcByName sc
  let env = Env { cfg = cfg, dapp = dappInfo' }
  -- start ui and run tests
  runReaderT (campaign (pure ()) v w ts (Just d) txs) env

testContract :: FilePath -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContract fp cfg = testContract' fp Nothing Nothing cfg True

testContractV :: FilePath -> Maybe SolcVersionComp -> Maybe FilePath -> [(String, Campaign -> Bool)] -> TestTree
testContractV fp v cfg = testContract' fp Nothing v cfg True

testContract' :: FilePath -> Maybe ContractName -> Maybe SolcVersionComp -> Maybe FilePath -> Bool -> [(String, Campaign -> Bool)] -> TestTree
testContract' fp n v configPath s as = testCase fp $ withSolcVersion v $ do
  c <- case configPath of
    Just path -> do
      parsed <- parseConfig path
      pure parsed.econfig
    Nothing -> pure testConfig
  let c' = c & overrideQuiet
             & (if s then overrideLimits else id)
  res <- runContract fp n c'
  mapM_ (\(t,f) -> assertBool t $ f res) as

checkConstructorConditions :: FilePath -> String -> TestTree
checkConstructorConditions fp as = testCase fp $ do
  (v, _, t) <- loadSolTests testConfig.solConf (fp :| []) Nothing
  let env = Env { cfg = testConfig, dapp = emptyDapp }
  r <- flip runReaderT env $
    mapM (\u -> evalStateT (checkETest u) v) t
  mapM_ (\(x,_) -> assertBool as (forceBool x)) r
  where forceBool (BoolValue b) = b
        forceBool _ = error "BoolValue expected"


getResult :: Text -> Campaign -> Maybe EchidnaTest
getResult n c =
  case filter findTest c._tests of
    []  -> Nothing
    [x] -> Just x
    _   -> error "found more than one tests"

  where findTest test = case test.testType  of
                          PropertyTest t _        -> t == n
                          AssertionTest _ (t,_) _ -> t == n
                          CallTest t _            -> t == n
                          OptimizationTest t _    -> t == n
                          _                       -> False

optnFor :: Text -> Campaign -> Maybe TestValue
optnFor n c = case getResult n c of
  Just t -> Just t.testValue
  _      -> Nothing

optimized :: Text -> Int256 -> Campaign -> Bool
optimized n v c = case optnFor n c of
                   Just (IntValue o1) -> o1 >= v
                   Nothing            -> error "nothing"
                   _                  -> error "incompatible values"

solnFor :: Text -> Campaign -> Maybe [Tx]
solnFor n c = case getResult n c of
  Just t -> if null t.testReproducer then Nothing else Just t.testReproducer
  _      -> Nothing

solved :: Text -> Campaign -> Bool
solved t = isJust . solnFor t

passed :: Text -> Campaign -> Bool
passed n c = case getResult n c of
  Just t | isPassed t -> True
  Just t | isOpen t   -> True
  Nothing             -> error ("no test was found with name: " ++ show n)
  _                   -> False

solvedLen :: Int -> Text -> Campaign -> Bool
solvedLen i t = (== Just i) . fmap length . solnFor t

solvedUsing :: Text -> Text -> Campaign -> Bool
solvedUsing f t = maybe False (any $ matchCall . (.call)) . solnFor t
                 where matchCall (SolCall (f',_)) = f' == f
                       matchCall _                = False

-- NOTE: this just verifies a call was found in the solution. Doesn't care about ordering/seq length
solvedWith :: TxCall -> Text -> Campaign -> Bool
solvedWith tx t = maybe False (any $ (== tx) . (.call)) . solnFor t

solvedWithout :: TxCall -> Text -> Campaign -> Bool
solvedWithout tx t = maybe False (all $ (/= tx) . (.call)) . solnFor t

getGas :: Text -> Campaign -> Maybe (Int, [Tx])
getGas t camp = lookup t camp._gasInfo

gasInRange :: Text -> Int -> Int -> Campaign -> Bool
gasInRange t l h c = case getGas t c of
  Just (g, _) -> g >= l && g <= h
  _           -> False

countCorpus :: Int -> Campaign -> Bool
countCorpus n c = length c._corpus == n

coverageEmpty :: Campaign -> Bool
coverageEmpty c = c._coverage == empty
