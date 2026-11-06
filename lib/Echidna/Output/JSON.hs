{-# LANGUAGE RecordWildCards #-}

module Echidna.Output.JSON where

import Data.Aeson hiding (Error)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as L
import Data.IORef (readIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text
import Data.Foldable (toList)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Vector.Unboxed qualified as VU
import Numeric (showHex)

import EVM.ABI (AbiValue(..))
import EVM.Dapp (DappInfo)

import Echidna.ABI (ppAbiValue, GenDict(..))
import Echidna.Events (Events, extractEvents)
import Echidna.Types.Campaign (WorkerState(..))
import Echidna.Types.Config (Env(..))
import Echidna.Types.Coverage (CoverageInfo, mergeCoverageMaps)
import Echidna.Types.Test (EchidnaTest(..))
import Echidna.Types.Test qualified as T
import Echidna.Types.Tx (Tx(..), TxCall(..), TxResult)

data Campaign = Campaign
  { _success :: Bool
  , _error :: Maybe String
  , _tests :: [Test]
  , seed :: Int
  , coverage :: Map String [CoverageInfo]
  }

instance ToJSON Campaign where
  toJSON Campaign{..} = object
    [ "success" .= _success
    , "error" .= _error
    , "tests" .= _tests
    , "seed" .= seed
    , "coverage" .= coverage
    ]

data Test = Test
  { contract :: Text
  , name :: Text
  , status :: TestStatus
  , _error :: Maybe String
  , reason :: Maybe TxResult
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
    , "reason" .= reason
    , "events" .= events
    , "type" .= testType
    , "transactions" .= transactions
    ]

data TestType = Property | Assertion

instance ToJSON TestType where
  toJSON Property = "property"
  toJSON Assertion = "assertion"

data TestStatus = Fuzzing | Shrinking | Solved | Verified | Passed | Error

instance ToJSON TestStatus where
  toJSON Fuzzing = "fuzzing"
  toJSON Verified = "verified"
  toJSON Shrinking = "shrinking"
  toJSON Solved = "solved"
  toJSON Passed = "passed"
  toJSON Error = "error"


data Transaction = Transaction
  { contract :: Text
  , function :: Text
  , arguments :: Maybe [Value]
  , gas :: String
  , gasprice :: String
  , value :: String
  }

instance ToJSON Transaction where
  toJSON Transaction{..} = object
    [ "contract" .= contract
    , "function" .= function
    , "arguments" .= arguments
    , "gas" .= gas
    , "gasprice" .= gasprice
    , "value" .= value
    ]

-- | Encode an 'AbiValue' as JSON. Integers, addresses, bools and functions keep
-- their textual rendering ('ppAbiValue') as a JSON string. @bytes@ and @bytesN@
-- are @0x@-prefixed hex strings, @string@ is a JSON string (hex like @bytes@ if
-- it is not valid UTF-8), and arrays and tuples are JSON arrays of encoded elements.
abiValueToJSON :: AbiValue -> Value
abiValueToJSON = \case
  AbiBytes _ b        -> hex b
  AbiBytesDynamic b   -> hex b
  AbiString s         -> either (const $ hex s) toJSON (decodeUtf8' s)
  AbiArrayDynamic _ v -> toJSON $ abiValueToJSON <$> toList v
  AbiArray _ _ v      -> toJSON $ abiValueToJSON <$> toList v
  AbiTuple v          -> toJSON $ abiValueToJSON <$> toList v
  v                   -> toJSON $ ppAbiValue mempty v
  where
  hex b = toJSON . decodeUtf8 $ "0x" <> BS16.encode b

encodeCampaign :: Env -> [WorkerState] -> IO L.ByteString
encodeCampaign env workerStates = do
  tests <- traverse readIORef env.testRefs
  frozenCov <- mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime
  -- TODO: this is ugly, refactor seed to live in Env
  let workerSeed [] = 0
      workerSeed (state:_) = state.genDict.defSeed
  let seed = workerSeed workerStates
  pure $ encode Campaign
    { _success = True
    , _error = Nothing
    , _tests = mapTest env.dapp <$> tests
    , seed = seed
    , coverage = Map.mapKeys (("0x" ++) . (`showHex` "")) $ VU.toList <$> frozenCov
    }

mapTest :: DappInfo -> EchidnaTest -> Test
mapTest dappInfo test =
  let (status, transactions, err) = mapTestState test.state test.reproducer
  in Test
    { contract = "" -- TODO add when mapping is available https://github.com/crytic/echidna/issues/415
    , name = "name" -- TODO add a proper name here
    , status = status
    , _error = err
    , reason = mapReason test.state
    , events = maybe [] (extractEvents False dappInfo) test.vm
    , testType = Property
    , transactions = transactions
    }
  where
  mapTestState T.Open _ = (Fuzzing, Nothing, Nothing)
  mapTestState T.Passed _ = (Passed, Nothing, Nothing)
  mapTestState T.Solved txs = (Solved, Just $ mapTx <$> txs, Nothing)
  mapTestState T.Unsolvable _ = (Verified, Nothing, Nothing)
  mapTestState (T.Large _) txs = (Shrinking, Just $ mapTx <$> txs, Nothing)
  mapTestState (T.Failed e) _ = (Error, Nothing, Just $ Prelude.show e) -- TODO add (show e)

  -- The reason a test failed is only meaningful for falsified tests.
  mapReason T.Solved    = Just test.result
  mapReason (T.Large _) = Just test.result
  mapReason _           = Nothing

  mapTx tx =
    let (function, args) = mapCall tx.call
    in Transaction
      { contract = "" -- TODO add when mapping is available https://github.com/crytic/echidna/issues/415
      , function = function
      , arguments = args
      , gas = Prelude.show tx.gas
      , gasprice = Prelude.show tx.gasprice
      , value = Prelude.show tx.value
      }

  mapCall = \case
    SolCreate _          -> ("<CREATE>", Nothing)
    SolCall (name, args) -> (name, Just $ abiValueToJSON <$> args)
    NoCall               -> ("*wait*", Nothing)
    SolCalldata x        -> (decodeUtf8 $ "0x" <> BS16.encode x, Nothing)
