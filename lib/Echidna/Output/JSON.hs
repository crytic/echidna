{-# LANGUAGE RecordWildCards #-}

module Echidna.Output.JSON where

import Data.Aeson hiding (Error)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as L
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Vector.Unboxed qualified as VU
import Numeric (showHex)

import EVM.Dapp (DappInfo)

import Echidna.ABI (ppAbiValue, GenDict(..))
import Echidna.Events (Events, extractEvents)
import Echidna.Types (Gas)
import Echidna.Types.Campaign (WorkerState(..))
import Echidna.Types.Config (Env(..))
import Echidna.Types.Coverage (CoverageInfo)
import Echidna.Types.Test qualified as T
import Echidna.Types.Test (EchidnaTest(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))

data Campaign = Campaign
  { _success :: Bool
  , _error :: Maybe String
  , _tests :: [Test]
  , seed :: Int
  , coverage :: Map String [CoverageInfo]
  , gasInfo :: [(Text, (Gas, [Tx]))]
  }

instance ToJSON Campaign where
  toJSON Campaign{..} = object
    [ "success" .= _success
    , "error" .= _error
    , "tests" .= _tests
    , "seed" .= seed
    , "coverage" .= coverage
    , "gas_info" .= gasInfo
    ]

data Test = Test
  { contract :: Text
  , name :: Text
  , status :: TestStatus
  , _error :: Maybe String
  , events :: Events
  , testType :: TestType
  , transactions :: Maybe [Transaction]
  }

instance ToJSON Test where
  toJSON Test{..} = object
    [ "contract" .= contract
    , "name" .= name
    , "status" .= status
    , "error" .= _error
    , "events" .= events
    , "type" .= testType
    , "transactions" .= transactions
    ]

data TestType = Property | Assertion

instance ToJSON TestType where
  toJSON Property = "property"
  toJSON Assertion = "assertion"

data TestStatus = Fuzzing | Shrinking | Solved | Passed | Error

instance ToJSON TestStatus where
  toJSON Fuzzing = "fuzzing"
  toJSON Shrinking = "shrinking"
  toJSON Solved = "solved"
  toJSON Passed = "passed"
  toJSON Error = "error"


data Transaction = Transaction
  { contract :: Text
  , function :: Text
  , arguments :: Maybe [String]
  , gas :: Integer
  , gasprice :: Integer
  }

instance ToJSON Transaction where
  toJSON Transaction{..} = object
    [ "contract" .= contract
    , "function" .= function
    , "arguments" .= arguments
    , "gas" .= gas
    , "gasprice" .= gasprice
    ]

encodeCampaign :: Env -> [WorkerState] -> IO L.ByteString
encodeCampaign env workerStates = do
  tests <- readIORef env.testsRef
  frozenCov <- mapM VU.freeze =<< readIORef env.coverageRef
  -- TODO: this is ugly, refactor seed to live in Env
  let worker0 = Prelude.head workerStates
  pure $ encode Campaign
    { _success = True
    , _error = Nothing
    , _tests = mapTest env.dapp <$> tests
    , seed = worker0.genDict.defSeed
    , coverage = Map.mapKeys (("0x" ++) . (`showHex` "")) $ VU.toList <$> frozenCov
    , gasInfo = Map.toList $ Map.unionsWith max ((.gasInfo) <$> workerStates)
    }

mapTest :: DappInfo -> EchidnaTest -> Test
mapTest dappInfo test =
  let (status, transactions, err) = mapTestState test.state test.reproducer
  in Test
    { contract = "" -- TODO add when mapping is available https://github.com/crytic/echidna/issues/415
    , name = "name" -- TODO add a proper name here
    , status = status
    , _error = err
    , events = maybe [] (extractEvents False dappInfo) test.vm
    , testType = Property
    , transactions = transactions
    }
  where
  mapTestState T.Open _ = (Fuzzing, Nothing, Nothing)
  mapTestState T.Passed _ = (Passed, Nothing, Nothing)
  mapTestState T.Solved txs = (Solved, Just $ mapTx <$> txs, Nothing)
  mapTestState (T.Large _) txs = (Shrinking, Just $ mapTx <$> txs, Nothing)
  mapTestState (T.Failed e) _ = (Error, Nothing, Just $ show e) -- TODO add (show e)

  mapTx tx =
    let (function, args) = mapCall tx.call
    in Transaction
      { contract = "" -- TODO add when mapping is available https://github.com/crytic/echidna/issues/415
      , function = function
      , arguments = args
      , gas = toInteger tx.gas
      , gasprice = toInteger tx.gasprice
      }

  mapCall = \case
    SolCreate _          -> ("<CREATE>", Nothing)
    SolCall (name, args) -> (name, Just $ ppAbiValue <$> args)
    NoCall               -> ("*wait*", Nothing)
    SolCalldata x        -> (decodeUtf8 $ "0x" <> BS16.encode x, Nothing)
