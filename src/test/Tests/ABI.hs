module Tests.ABI (abiTests) where

import Control.Monad (when)
import Control.Monad.State.Strict (evalStateT)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Vector qualified as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import EVM.ABI (AbiType(..), AbiValue(..), abiValueType)

import Echidna.ABI (coerceAbiValue, coercePrototype)
import Echidna.MCP.Parse (parseArg, parseFuzzCall)
import Echidna.Transaction (genPrototypeCall, matchingContracts)
import Echidna.Types.Campaign (initialWorkerState)

abiTests :: TestTree
abiTests = testGroup "ABI coercion"
  [ testGroup "integer bounds"
      [ testCase (show bits <> " bits") $ do
          let umax = 2 ^ bits - 1 :: Integer
              imin = negate (2 ^ (bits - 1)) :: Integer
              imax = 2 ^ (bits - 1) - 1 :: Integer
              check t n = parseAndCoerce t (show (n :: Integer))
          check (AbiUIntType bits) 0 @?= Right (AbiUInt bits 0)
          check (AbiUIntType bits) umax @?= Right (AbiUInt bits (fromInteger umax))
          check (AbiIntType bits) imin @?= Right (AbiInt bits (fromInteger imin))
          check (AbiIntType bits) imax @?= Right (AbiInt bits (fromInteger imax))
          assertBool "rejects negative unsigned values" $ isLeft (check (AbiUIntType bits) (-1))
          assertBool "rejects unsigned overflow" $ isLeft (check (AbiUIntType bits) (umax + 1))
          assertBool "rejects signed underflow" $ isLeft (check (AbiIntType bits) (imin - 1))
          assertBool "rejects signed overflow" $ isLeft (check (AbiIntType bits) (imax + 1))
      | bits <- [8, 32, 128, 160, 256]
      ]
  , testCase "hex works for uint, signed int and address" $ do
      parseAndCoerce (AbiUIntType 8) "0xff" @?= Right (AbiUInt 8 255)
      parseAndCoerce (AbiIntType 128) "-0x5" @?= Right (AbiInt 128 (-5))
      parseAndCoerce AbiAddressType "0x10" @?= Right (AbiAddress 16)
      parseAndCoerce AbiAddressType (show (2 ^ (160 :: Int) - 1 :: Integer)) @?=
        Right (AbiAddress (fromInteger (2 ^ (160 :: Int) - 1)))
      assertBool "rejects address truncation" $ isLeft $
        parseAndCoerce AbiAddressType (show (2 ^ (160 :: Int) :: Integer))
  , testCase "bool values and bounds" $ do
      parseAndCoerce AbiBoolType "true" @?= Right (AbiBool True)
      parseAndCoerce AbiBoolType "0" @?= Right (AbiBool False)
      parseAndCoerce AbiBoolType "1" @?= Right (AbiBool True)
      assertBool "rejects non-boolean numbers" $ isLeft (parseAndCoerce AbiBoolType "2")
      assertBool "rejects boolean as integer" $ isLeft (parseAndCoerce (AbiUIntType 8) "true")
  , testCase "bytes preserve the full word and pad within the declared width" $ do
      parseAndCoerce (AbiBytesType 32) ("0x" <> replicate 64 'f') @?=
        Right (AbiBytes 32 (BS.replicate 32 255))
      parseAndCoerce (AbiBytesType 4) "0x1234" @?=
        Right (AbiBytes 4 (BS.pack [0, 0, 0x12, 0x34]))
      assertBool "rejects bytes truncation" $ isLeft (parseAndCoerce (AbiBytesType 1) "256")
      assertBool "rejects negative bytes" $ isLeft (parseAndCoerce (AbiBytesType 32) "-1")
  , testCase "arrays coerce all elements and fixed lengths" $ do
      parseAndCoerce (AbiArrayDynamicType (AbiIntType 8)) "[-5, 11]" @?=
        Right (AbiArrayDynamic (AbiIntType 8) (V.fromList [AbiInt 8 (-5), AbiInt 8 11]))
      parseAndCoerce (AbiArrayType 2 (AbiUIntType 32)) "[1, 2]" @?=
        Right (AbiArray 2 (AbiUIntType 32) (V.fromList [AbiUInt 32 1, AbiUInt 32 2]))
      parseAndCoerce (AbiArrayDynamicType AbiAddressType) "[]" @?=
        Right (AbiArrayDynamic AbiAddressType V.empty)
      parseAndCoerce (AbiArrayDynamicType (AbiArrayType 1 (AbiUIntType 8))) "[[7]]" @?=
        Right (AbiArrayDynamic (AbiArrayType 1 (AbiUIntType 8))
          (V.singleton (AbiArray 1 (AbiUIntType 8) (V.singleton (AbiUInt 8 7)))))
      assertBool "rejects wrong array length" $ isLeft $
        parseAndCoerce (AbiArrayType 2 (AbiUIntType 8)) "[1]"
      assertBool "checks every element" $ isLeft $
        parseAndCoerce (AbiArrayDynamicType (AbiUIntType 8)) "[1, 300]"
      assertBool "rejects incompatible elements" $ isLeft $
        parseAndCoerce (AbiArrayDynamicType (AbiUIntType 8)) "[1, true]"
  , testCase "prototype preserves holes and validates name and arity" $ do
      let sig = ("give", [AbiUIntType 8, AbiUIntType 8, AbiUIntType 128])
          prototype = ("give", [Just (AbiUInt 256 0), Nothing, Just (AbiUInt 256 1000)])
      coercePrototype sig prototype @?=
        Right ("give", [Just (AbiUInt 8 0), Nothing, Just (AbiUInt 128 1000)])
      assertBool "rejects wrong name" $ isLeft (coercePrototype sig ("other", snd prototype))
      assertBool "rejects wrong arity" $ isLeft (coercePrototype sig ("give", []))
  , testCase "mixed fuzz prototypes use declared types for concrete arguments" $
      checkGenerated "give(0, ?, 1000)" True
  , testCase "uncoercible fuzz arguments are generated without losing valid ones" $
      checkGenerated "give(300, ?, 1000)" False
  ]
  where
    parseAndCoerce t literal = case parseArg literal of
      Nothing -> Left "Could not parse literal."
      Just v -> coerceAbiValue t v

    checkGenerated literal fixedFirst = do
      prototype <- maybe (assertFailure "Could not parse prototype") pure (parseFuzzCall literal)
      let types = [AbiUIntType 8, AbiUIntType 8, AbiUIntType 128]
          candidates = matchingContracts prototype [(0x10, ("give", types) :| [])]
      (addr, (name, vals)) <- evalStateT (genPrototypeCall prototype candidates) initialWorkerState
      addr @?= 0x10
      name @?= "give"
      map abiValueType vals @?= types
      case vals of
        [a, b, c] -> do
          when fixedFirst $ a @?= AbiUInt 8 0
          coerceAbiValue (AbiUIntType 8) a @?= Right a
          coerceAbiValue (AbiUIntType 8) b @?= Right b
          c @?= AbiUInt 128 1000
        _ -> assertFailure "Expected three arguments."
