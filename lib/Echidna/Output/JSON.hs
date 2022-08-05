{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.Output.JSON where

import Control.Lens ((^.))
import Echidna.ABI (ppAbiValue, GenDict(..))
import Echidna.Types.Coverage (CoverageInfo)
import qualified Echidna.Types.Campaign as C
import qualified Echidna.Types.Test as T
import Echidna.Types.Test (EchidnaTest, testState, testReproducer)
import Echidna.Types.Tx (Tx(..), TxCall(..))
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Base16 as BS16
import Data.ByteString.Lazy (ByteString)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.Map
import EVM.Types (keccak)
import qualified Data.Foldable as DF
import Numeric (showHex)

data Campaign = Campaign
  { _success :: Bool
  , _error :: Maybe String
  , _tests :: [Test]
  , seed :: Int
  , coverage :: Map String [CoverageInfo]
  , gasInfo :: [(Text, (Int, [Tx]))]
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
  , testType :: TestType
  , transactions :: Maybe [Transaction]
  }

instance ToJSON Test where
  toJSON Test{..} = object
    [ "contract" .= contract
    , "name" .= name
    , "status" .= status
    , "error" .= _error
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

encodeCampaign :: C.Campaign -> ByteString
encodeCampaign C.Campaign{..} = encode
  Campaign { _success = True
           , _error = Nothing
           , _tests = mapTest <$> _tests
           , seed = _defSeed _genDict
           , coverage = mapKeys (("0x" ++) . (`showHex` "") . keccak) $ DF.toList <$> _coverage
           , gasInfo = toList _gasInfo
           }

mapTest :: EchidnaTest -> Test
mapTest echidnaTest =
  let tst = echidnaTest ^. testState
      txs = echidnaTest ^. testReproducer 
      (status, transactions, err) = mapTestState tst txs in
  Test { contract = "" -- TODO add when mapping is available https://github.com/crytic/echidna/issues/415
       , name = "name" --TODO add a proper name here
       , status = status
       , _error = err
       , testType = Property 
       , transactions = transactions
       }
  where
  mapTestState (T.Open _) _ = (Fuzzing, Nothing, Nothing)
  mapTestState T.Passed _ = (Passed, Nothing, Nothing)
  mapTestState T.Solved txs = (Solved, Just $ mapTx <$> txs, Nothing)
  mapTestState (T.Large _) txs = (Shrinking, Just $ mapTx <$> txs, Nothing)
  mapTestState (T.Failed e) _ = (Error, Nothing, Just $ show e) -- TODO add (show e)

  mapTx Tx{..} =
    let (function, args) = mapCall _call in
    Transaction { contract = "" -- TODO add when mapping is available https://github.com/crytic/echidna/issues/415
                , function = function
                , arguments = args
                , gas = toInteger _gas'
                , gasprice = toInteger _gasprice'
                }

  mapCall (SolCreate _) = ("<CREATE>", Nothing)
  mapCall (SolCall (name, args)) = (name, Just $ ppAbiValue <$> args)
  mapCall NoCall                 = ("*wait*", Nothing)
  mapCall (SolCalldata x) = (decodeUtf8 $ "0x" <> BS16.encode x, Nothing)
