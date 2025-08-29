module Echidna.Output.Foundry where

import Data.List (nub)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector as V hiding ((++), map, zipWith)
import EVM.ABI (AbiValue(..))
import Echidna.ABI ()
import EVM.Types (W256, Addr)
import Numeric (showHex)

import Echidna.Types.Test (EchidnaTest(..), TestType(..))
import Echidna.Types.Tx (Tx(..), TxCall(..))

foundryTestHeader :: String
foundryTestHeader = unlines
  [ "// SPDX-License-Identifier: Unlicense"
  , "pragma solidity ^0.8.0;"
  , ""
  , "import \"forge-std/Test.sol\";"
  , ""
  ]

foundryTestBody :: Maybe Text -> String -> EchidnaTest -> String
foundryTestBody mContractName reproducerHash test =
  case test.testType of
    AssertionTest{} ->
      let
        contractName = "Test." ++ reproducerHash
        senders = nub $ map (.src) test.reproducer
        actors = foundryActors senders
        repro = foundryReproducer test
        cName = maybe "YourContract" unpack mContractName
      in unlines
        [ "contract " ++ contractName ++ " is Test {"
        , actors
        , "    // TODO: Replace with your actual contract instance"
        , "    " ++ cName ++ " Tester;"
        , ""
        , "  function setUp() public {"
        , "      // TODO: Initialize your contract here"
        , "      Tester = new " ++ cName ++ "();"
        , "  }"
        , ""
        , "  function test_replay() public {"
        , repro
        , "  }"
        , ""
        , "  function _setUpActor(address actor) internal {"
        , "      vm.startPrank(actor);"
        , "      // Add any additional actor setup here if needed"
        , "  }"
        , ""
        , "  function _delay(uint256 timeInSeconds) internal {"
        , "      vm.warp(block.timestamp + timeInSeconds);"
        , "  }"
        , "}"
        ]
    _ -> ""

foundryActors :: [Addr] -> String
foundryActors senders = unlines $ zipWith foundryActor senders [1..]

foundryActor :: Addr -> Int -> String
foundryActor sender i = "    address constant USER" ++ show i ++ " = " ++ formatAddr sender ++ ";"

formatAddr :: Addr -> String
formatAddr addr = "address(0x" ++ showHex (fromIntegral addr :: W256) "" ++ ")"

foundryTest :: Maybe Text -> String -> EchidnaTest -> String
foundryTest mContractName reproducerHash test = foundryTestHeader ++ foundryTestBody mContractName reproducerHash test

foundryReproducer :: EchidnaTest -> String
foundryReproducer test = unlines $ map foundryTx test.reproducer

foundryTx :: Tx -> String
foundryTx tx =
  case tx.call of
    SolCall (name, args) ->
      let
        sender = "_setUpActor(address(0x" ++ showHex (fromIntegral tx.src :: W256) "" ++ "));"
        call = "Tester." ++ unpack name ++ "(" ++ foundryArgs (map abiValueToString args) ++ ");"
      in "    " ++ sender ++ "\n" ++ "    " ++ call
    _ -> ""

foundryArgs :: [String] -> String
foundryArgs [] = ""
foundryArgs [x] = x
foundryArgs (x:xs) = x ++ ", " ++ foundryArgs xs

abiValueToString :: AbiValue -> String
abiValueToString (AbiUInt _ w) = show w
abiValueToString (AbiInt _ w) = show w
abiValueToString (AbiAddress a) = "address(0x" ++ showHex (fromIntegral a :: W256) "" ++ ")"
abiValueToString (AbiBool b) = if b then "true" else "false"
abiValueToString (AbiBytes _ bs) = "hex\"" ++ unpack (decodeUtf8 bs) ++ "\""
abiValueToString (AbiString s) = show s
abiValueToString (AbiTuple vs) = "(" ++ foundryArgs (map abiValueToString (V.toList vs)) ++ ")"
abiValueToString _ = ""
