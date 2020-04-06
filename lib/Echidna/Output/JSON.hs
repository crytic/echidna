{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.Output.JSON where

import Echidna.ABI (ppAbiValue)
import qualified Echidna.Campaign as C
import Echidna.Solidity (SolTest)
import Echidna.Transaction (Tx(..), TxCall(..))
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Base16 as BS16
import Data.ByteString.Lazy (ByteString)
import Data.Text
import Data.Text.Encoding (decodeUtf8)

data Top = Top
  { _success :: Bool
  , _error :: Maybe String
  , _tests :: [Test]
  , seed :: Int
  }

instance ToJSON Top where
  toJSON Top{..} = object
    [ "success" .= _success
    , "error" .= _error
    , "tests" .= _tests
    , "seed" .= seed
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

encodeCampaign :: C.Campaign -> Int -> ByteString
encodeCampaign C.Campaign{_tests} seed = encode
  -- TODO: success and error are hardcoded, add after reworking error reporting
  Top { _success = True
      , _error = Nothing
      , _tests = mapTest <$> _tests
      , seed = seed
      }

mapTest :: (SolTest, C.TestState) -> Test
mapTest (solTest, testState) =
  let (status, transactions, err) = mapTestState testState in
  Test { contract = "" -- TODO add when mapping is available
       , name = case solTest of Left (n, _) -> n; Right (n, _) -> n
       , status = status
       , _error = err
       , testType = case solTest of Left _ -> Property; Right _ -> Assertion
       , transactions = transactions
       }
  where
  mapTestState (C.Open _) = (Fuzzing, Nothing, Nothing)
  mapTestState C.Passed = (Passed, Nothing, Nothing)
  mapTestState (C.Solved txs) = (Solved, Just $ mapTx <$> txs, Nothing)
  mapTestState (C.Large _ txs) = (Shrinking, Just $ mapTx <$> txs, Nothing)
  mapTestState (C.Failed e) = (Error, Nothing, Just $ show e) -- TODO add (show e)

  mapTx Tx{..} =
    let (function, args) = mapCall _call in
    Transaction { contract = "" -- TODO add when mapping is available
                , function = function
                , arguments = args
                , gas = toInteger _gas'
                , gasprice = toInteger _gasprice'
                }

  mapCall (SolCreate _) = ("<CREATE>", Nothing)
  mapCall (SolCall (name, args)) = (name, Just $ ppAbiValue <$> args)
  mapCall (SolCalldata x) = (decodeUtf8 $ "0x" <> BS16.encode x, Nothing)
