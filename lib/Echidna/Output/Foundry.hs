{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Echidna.Output.Foundry (foundryTest) where

import Data.Aeson (Value(..), object, (.=))
import Data.List (elemIndex, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (fromStrict)
import Data.Vector as V hiding ((++), map, zipWith, elemIndex, mapMaybe)
import EVM.ABI (AbiValue(..))
import EVM.Types (W256, Addr)
import Numeric (showHex)
import Text.Mustache (Template, substituteValue, toMustache)
import Text.Mustache.Compile (embedTemplate)

import Echidna.Types.Test (EchidnaTest(..), TestType(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))

template :: Template
template = $(embedTemplate ["lib/Echidna/Output/assets"] "foundry.mustache")

-- | Generate a Foundry test from an EchidnaTest result.
foundryTest :: Maybe Text -> EchidnaTest -> TL.Text
foundryTest mContractName test =
  case test.testType of
    AssertionTest{} ->
      let testData = createTestData mContractName test
      in fromStrict $ substituteValue template (toMustache testData)
    _ -> ""

-- | Create an Aeson Value from test data for the Mustache template.
createTestData :: Maybe Text -> EchidnaTest -> Value
createTestData mContractName test =
  let
    senders = nub $ map (.src) test.reproducer
    actors = zipWith actorObject senders [1..]
    repro = mapMaybe (foundryTx senders) test.reproducer
    cName = fromMaybe "YourContract" mContractName
  in
  object
    [ "testName"     .= ("Test" :: Text)
    , "contractName" .= cName
    , "actors"       .= actors
    , "reproducer"   .= repro
    ]

-- | Create a JSON object for an actor.
actorObject :: Addr -> Int -> Value
actorObject sender i = object
  [ "name"    .= ("USER" ++ show i :: String)
  , "address" .= formatAddr sender
  ]

-- | Format an address for Solidity.
formatAddr :: Addr -> String
formatAddr addr = "address(0x" ++ showHex (fromIntegral addr :: W256) "" ++ ")"

-- | Generate a single transaction line for the reproducer.
foundryTx :: [Addr] -> Tx -> Maybe Value
foundryTx senders tx =
  case tx.call of
    SolCall (name, args) ->
      let
        (time, blocks) = tx.delay
        senderName =
          case elemIndex tx.src senders of
            Just i -> "USER" ++ show (i + 1)
            Nothing -> formatAddr tx.src
        prelude =
          (if time > 0 || blocks > 0 then "    _delay(" ++ show time ++ ", " ++ show blocks ++ ");\n" else "") ++
          "    _setUpActor(" ++ senderName ++ ");"
        call = "    Target." ++ unpack name ++ "(" ++ foundryArgs (map abiValueToString args) ++ ");"
      in Just $ object ["prelude" .= prelude, "call" .= call]
    _ -> Nothing

-- | Format arguments for a Solidity call.
foundryArgs :: [String] -> String
foundryArgs [] = ""
foundryArgs [x] = x
foundryArgs (x:xs) = x ++ ", " ++ foundryArgs xs

-- | Convert an AbiValue to its string representation for Solidity.
abiValueToString :: AbiValue -> String
abiValueToString (AbiUInt _ w) = show w
abiValueToString (AbiInt _ w) = show w
abiValueToString (AbiAddress a) = "address(0x" ++ showHex (fromIntegral a :: W256) "" ++ ")"
abiValueToString (AbiBool b) = if b then "true" else "false"
abiValueToString (AbiBytes _ bs) = "hex\"" ++ unpack (decodeUtf8 bs) ++ "\""
abiValueToString (AbiString s) = show s
abiValueToString (AbiTuple vs) = "(" ++ foundryArgs (map abiValueToString (V.toList vs)) ++ ")"
abiValueToString _ = ""
