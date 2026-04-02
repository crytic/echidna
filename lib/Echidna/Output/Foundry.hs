{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Echidna.Output.Foundry (foundryTest) where

import Data.Aeson (Value(..), object, (.=))
import Data.Functor ((<&>))
import Data.List (elemIndex, isPrefixOf, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, unpack)
import Data.Text.Lazy (fromStrict)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import Data.Vector as V hiding ((++), map, zipWith, elemIndex, mapMaybe, concatMap, length)
import Numeric (showHex)
import qualified Data.Text.Lazy as TL
import Text.Mustache (Template, substituteValue, toMustache)
import Text.Mustache.Compile (embedTemplate)

import EVM.ABI (AbiValue(..))
import EVM.Types (W256, Addr)

import Echidna.Types.Test (EchidnaTest(..), TestType(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))

template :: Template
template = $(embedTemplate ["lib/Echidna/Output/assets"] "foundry.mustache")

-- | Generate a Foundry test from an EchidnaTest result.
-- For property tests, psender is the address used to call the property function.
foundryTest :: Maybe Text -> Addr -> EchidnaTest -> TL.Text
foundryTest mContractName psender test =
  case test.testType of
    AssertionTest{} ->
      let testData = createTestData mContractName Nothing Nothing test
      in fromStrict $ substituteValue template (toMustache testData)
    PropertyTest name _ ->
      let testData = createTestData mContractName (Just (name, psender)) Nothing test
      in fromStrict $ substituteValue template (toMustache testData)
    CallTest name _ | "AssertionFailed" `isPrefixOf` unpack name ->
      -- Echidna detects assertion failures via events named AssertionFailed
      -- with any argument types (see checkAssertionEvent in Echidna.Test).
      -- We check all overloads defined in crytic's fuzzlib (LibLog.sol):
      --   AssertionFailed()
      --   AssertionFailed(string)
      --   AssertionFailed(string,string)
      --   AssertionFailed(string,bytes)
      --   AssertionFailed(string,uint256)
      --   AssertionFailed(string,int256)
      --   AssertionFailed(string,address)
      --   AssertionFailed(string,bool)
      --   AssertionFailed(string,bytes32)
      let eventAssert = Just $
            "        // Check that an AssertionFailed event was emitted\n"
            ++ "        Vm.Log[] memory entries = vm.getRecordedLogs();\n"
            ++ "        bool found = false;\n"
            ++ "        for (uint i = 0; i < entries.length; i++) {\n"
            ++ "            if (entries[i].topics.length > 0 && _isAssertionFailed(entries[i].topics[0])) {\n"
            ++ "                found = true;\n"
            ++ "                break;\n"
            ++ "            }\n"
            ++ "        }\n"
            ++ "        assertTrue(found, \"Expected AssertionFailed event\");"
          testData = createTestData mContractName Nothing eventAssert test
      in fromStrict $ substituteValue template (toMustache testData)
    _ -> ""

-- | Create an Aeson Value from test data for the Mustache template.
-- When a property name and psender are provided, a final assertion is added
-- to call the property from psender and check it returns false.
-- When an event assertion is provided, vm.recordLogs() is added at the start
-- and the event check is added at the end.
createTestData :: Maybe Text -> Maybe (Text, Addr) -> Maybe String -> EchidnaTest -> Value
createTestData mContractName mProperty mEventAssert test =
  let
    senders = nub $ map (.src) test.reproducer
    actors = zipWith actorObject senders [1..]
    repro = mapMaybe (foundryTx senders) test.reproducer
    cName = fromMaybe "YourContract" mContractName
    propAssertion = case mProperty of
      Just (name, addr) -> Just $
        "        vm.stopPrank();\n        vm.prank(" ++ formatAddr addr ++ ");\n"
        ++ "        assertFalse(Target." ++ unpack name ++ "());"
      Nothing -> mEventAssert
    preamble = case mEventAssert of
      Just _ -> Just ("        vm.recordLogs();" :: String)
      Nothing -> Nothing
  in
  object
    [ "testName"     .= ("FoundryTest" :: Text)
    , "contractName" .= cName
    , "actors"       .= actors
    , "reproducer"   .= repro
    , "propertyAssertion" .= propAssertion
    , "preamble"     .= preamble
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
        -- Handle fallback function (empty name).
        call = if unpack name == ""
          then "    address(Target).call(\"\");"
          else "    Target." ++ unpack name ++ "(" ++ foundryArgs (map abiValueToString args) ++ ");"
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
abiValueToString (AbiBytes _ bs) = "hex\"" ++ C8.unpack (B16.encode bs) ++ "\""
abiValueToString (AbiString s) = show s
abiValueToString (AbiTuple vs) = "(" ++ foundryArgs (map abiValueToString (V.toList vs)) ++ ")"
abiValueToString _ = ""
