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
  , overrideQuiet
  , loadSolTests
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertBool)

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Random (getRandomR)
import Control.Monad.ST (RealWorld)
import Data.DoubleWord (Int256)
import Data.Function ((&))
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.SemVer (Version, version, fromText)
import Data.Text (Text, pack)
import System.Process (readProcess)

import Echidna (mkEnv, prepareContract)
import Echidna.Config (parseConfig, defaultConfig)
import Echidna.Campaign (runWorker)
import Echidna.Solidity (selectMainContract, mkTests, loadSpecified, compileContracts)
import Echidna.Test (checkETest)
import Echidna.Types (Gas)
import Echidna.Types.Config (Env(..), EConfig(..), EConfigWithUsage(..))
import Echidna.Types.Campaign
import Echidna.Types.Signature (ContractName)
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Test
import Echidna.Types.Tx (Tx(..), TxCall(..), call)
import Echidna.Types.World (World(..))

import EVM.Solidity (Contracts(..), BuildOutput(..), SolcContract(..))
import EVM.Types hiding (Env, Gas)

testConfig :: EConfig
testConfig = defaultConfig & overrideQuiet
                           & overrideLimits

overrideQuiet :: EConfig -> EConfig
overrideQuiet conf =
  conf { solConf = conf.solConf { quiet = True }}

overrideLimits :: EConfig -> EConfig
overrideLimits conf =
  conf { campaignConf = conf.campaignConf { testLimit = 10000
                                          , shrinkLimit = 4000 }}

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

runContract :: FilePath -> Maybe ContractName -> EConfig -> WorkerType -> IO (Env, WorkerState)
runContract f selectedContract cfg workerType = do
  seed <- maybe (getRandomR (0, maxBound)) pure cfg.campaignConf.seed
  buildOutput <- compileContracts cfg.solConf (f :| [])

  (vm, env, dict) <- prepareContract cfg (f :| []) buildOutput selectedContract seed

  (_stopReason, finalState) <- flip runReaderT env $
    runWorker workerType (pure ()) vm dict 0 [] cfg.campaignConf.testLimit selectedContract

  -- TODO: consider snapshotting the state so checking function don't need to
  -- be IO
  pure (env, finalState)

testContract
  :: FilePath
  -> Maybe FilePath
  -> [(String, (Env, WorkerState) -> IO Bool)]
  -> TestTree
testContract fp cfg = testContract' fp Nothing Nothing cfg True FuzzWorker

testContractV
  :: FilePath
  -> Maybe SolcVersionComp
  -> Maybe FilePath
  -> [(String, (Env, WorkerState) -> IO Bool)]
  -> TestTree
testContractV fp v cfg = testContract' fp Nothing v cfg True FuzzWorker

testContract'
  :: FilePath
  -> Maybe ContractName
  -> Maybe SolcVersionComp
  -> Maybe FilePath
  -> Bool
  -> WorkerType
  -> [(String, (Env, WorkerState) -> IO Bool)]
  -> TestTree
testContract' fp n v configPath s workerType expectations = testCase fp $ withSolcVersion v $ do
  c <- case configPath of
    Just path -> do
      parsed <- parseConfig path
      pure parsed.econfig
    Nothing -> pure testConfig
  let c' = c & overrideQuiet
             & (if s then overrideLimits else id)
  result <- runContract fp n c' workerType
  forM_ expectations $ \(message, assertion) -> do
    assertion result >>= assertBool message

-- | Given a file and an optional contract name, compile the file as solidity, then, if a name is
-- given, try to fine the specified contract (assuming it is in the file provided), otherwise, find
-- the first contract in the file. Take said contract and return an initial VM state with it loaded,
-- its ABI (as 'SolSignature's), and the names of its Echidna tests. NOTE: unlike 'loadSpecified',
-- contract names passed here don't need the file they occur in specified.
loadSolTests
  :: EConfig
  -> BuildOutput
  -> Maybe Text
  -> IO (VM Concrete RealWorld, Env, [EchidnaTest])
loadSolTests cfg buildOutput name = do
  let solConf = cfg.solConf
      (Contracts contractMap) = buildOutput.contracts
      contracts = Map.elems contractMap
      eventMap = Map.unions $ map (.eventMap) contracts
      world = World solConf.sender mempty Nothing [] eventMap
  mainContract <- selectMainContract solConf name contracts
  echidnaTests <- mkTests solConf mainContract
  env <- mkEnv cfg buildOutput echidnaTests world Nothing
  vm <- loadSpecified env mainContract contracts
  pure (vm, env, echidnaTests)

checkConstructorConditions :: FilePath -> String -> TestTree
checkConstructorConditions fp as = testCase fp $ do
  let cfg = testConfig
  buildOutput <- compileContracts cfg.solConf (pure fp)
  (v, env, t) <- loadSolTests cfg buildOutput Nothing
  r <- flip runReaderT env $ mapM (`checkETest` v) t
  mapM_ (\(x,_) -> assertBool as (forceBool x)) r
  where forceBool (BoolValue b) = b
        forceBool _ = error "BoolValue expected"

getResult :: Text -> [EchidnaTest] -> Maybe EchidnaTest
getResult n tests =
  case filter findTest tests of
    []  -> Nothing
    [x] -> Just x
    _   -> error "found more than one tests"

  where findTest test = case test.testType  of
                          PropertyTest t _        -> t == n
                          AssertionTest _ (t,_) _ -> t == n
                          CallTest t _            -> t == n
                          OptimizationTest t _    -> t == n
                          _                       -> False

optnFor :: Text -> (Env, WorkerState) -> IO (Maybe TestValue)
optnFor n (env, _) = do
  tests <- traverse readIORef env.testRefs
  pure $ case getResult n tests of
    Just t -> Just t.value
    _      -> Nothing

optimized :: Text -> Int256 -> (Env, WorkerState) -> IO Bool
optimized n v final = do
  x <- optnFor n final
  pure $ case x of
    Just (IntValue o1) -> o1 >= v
    Nothing            -> error "nothing"
    _                  -> error "incompatible values"

solnFor :: Text -> (Env, WorkerState) -> IO (Maybe [Tx])
solnFor n (env, _) = do
  tests <- traverse readIORef env.testRefs
  pure $ case getResult n tests of
    Just t -> if null t.reproducer then Nothing else Just t.reproducer
    _      -> Nothing

solved :: Text -> (Env, WorkerState) -> IO Bool
solved t f = isJust <$> solnFor t f

passed :: Text -> (Env, WorkerState) -> IO Bool
passed n (env, _) = do
  tests <- traverse readIORef env.testRefs
  pure $ case getResult n tests of
    Just t | isPassed t -> True
    Just t | isOpen t   -> True
    Nothing             -> error ("no test was found with name: " ++ show n)
    _                   -> False

solvedLen :: Int -> Text -> (Env, WorkerState) -> IO Bool
solvedLen i t final = (== Just i) . fmap length <$> solnFor t final

solvedUsing :: Text -> Text -> (Env, WorkerState) -> IO Bool
solvedUsing f t final =
  maybe False (any $ matchCall . (.call)) <$> solnFor t final
  where matchCall (SolCall (f',_)) = f' == f
        matchCall _                = False

-- NOTE: this just verifies a call was found in the solution. Doesn't care about ordering/seq length
solvedWith :: TxCall -> Text -> (Env, WorkerState) -> IO Bool
solvedWith tx t final =
  maybe False (any $ (== tx) . (.call)) <$> solnFor t final

solvedWithout :: TxCall -> Text -> (Env, WorkerState) -> IO Bool
solvedWithout tx t final =
  maybe False (all $ (/= tx) . (.call)) <$> solnFor t final

getGas :: Text -> WorkerState -> Maybe (Gas, [Tx])
getGas t camp = Map.lookup t camp.gasInfo

gasInRange :: Text -> Gas -> Gas -> (Env, WorkerState) -> IO Bool
gasInRange t l h (_, workerState) = do
  pure $ case getGas t workerState of
    Just (g, _) -> g >= l && g <= h
    _           -> False

countCorpus :: Int -> (Env, WorkerState) -> IO Bool
countCorpus n (env, _) = do
  corpus <- readIORef env.corpusRef
  pure $ length corpus == n
