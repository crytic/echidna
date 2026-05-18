module Tests.MCPParse (mcpParseTests) where

import Data.Maybe (isNothing)
import Data.Vector qualified as Vector
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import EVM.ABI (AbiType(..), AbiValue(..))

import Echidna.MCP
  ( parseArg
  , parseArray
  , parseCall
  , parseFuzzArg
  , parseFuzzCall
  , parseFuzzSequence
  , parsePrimitive
  , splitArgs
  )

uint :: Integer -> AbiValue
uint = AbiUInt 256 . fromInteger

addr :: Integer -> AbiValue
addr = AbiAddress . fromInteger

uintArray :: [Integer] -> AbiValue
uintArray xs =
  AbiArrayDynamic (AbiUIntType 256) (Vector.fromList (map uint xs))

mcpParseTests :: TestTree
mcpParseTests = testGroup "MCP parsing"
  [ primitiveTests
  , arrayTests
  , argTests
  , fuzzArgTests
  , fuzzCallTests
  , fuzzSequenceTests
  , callTests
  , splitArgsTests
  ]

primitiveTests :: TestTree
primitiveTests = testGroup "parsePrimitive"
  [ testCase "true"  $ parsePrimitive "true"  @?= Just (AbiBool True)
  , testCase "false" $ parsePrimitive "false" @?= Just (AbiBool False)
  , testCase "True (mixed case)" $ parsePrimitive "True"  @?= Just (AbiBool True)
  , testCase "FALSE (upper case)" $ parsePrimitive "FALSE" @?= Just (AbiBool False)
  , testCase "decimal integer" $ parsePrimitive "42" @?= Just (uint 42)
  , testCase "zero" $ parsePrimitive "0" @?= Just (uint 0)
  , testCase "hex address" $ parsePrimitive "0x1234" @?= Just (addr 0x1234)
  , testCase "trims surrounding whitespace" $
      parsePrimitive "  42  " @?= Just (uint 42)
  , testCase "non-numeric garbage rejected" $
      assertBool "expected Nothing" (isNothing (parsePrimitive "abc"))
  , testCase "empty string rejected" $
      assertBool "expected Nothing" (isNothing (parsePrimitive ""))
  ]

arrayTests :: TestTree
arrayTests = testGroup "parseArray"
  [ testCase "three uints" $
      parseArray "[1,2,3]" @?= Just (uintArray [1, 2, 3])
  , testCase "empty array defaults to uint256 element type" $
      parseArray "[]" @?=
        Just (AbiArrayDynamic (AbiUIntType 256) Vector.empty)
  , testCase "tolerates whitespace around elements" $
      parseArray "[ 1 , 2 ]" @?= Just (uintArray [1, 2])
  , testCase "bool array" $
      parseArray "[true, false]" @?=
        Just (AbiArrayDynamic AbiBoolType
                (Vector.fromList [AbiBool True, AbiBool False]))
  , testCase "rejects mixed element types" $
      assertBool "expected Nothing" (isNothing (parseArray "[1, true]"))
  ]

argTests :: TestTree
argTests = testGroup "parseArg"
  [ testCase "dispatches to array when bracketed" $
      parseArg "[1,2]" @?= Just (uintArray [1, 2])
  , testCase "dispatches to primitive otherwise" $
      parseArg "42" @?= Just (uint 42)
  , testCase "non-numeric garbage rejected" $
      assertBool "expected Nothing" (isNothing (parseArg "abc"))
  ]

fuzzArgTests :: TestTree
fuzzArgTests = testGroup "parseFuzzArg"
  [ testCase "'?' means leave for the fuzzer" $
      parseFuzzArg "?" @?= Just Nothing
  , testCase "'?' surrounded by whitespace still means fuzz" $
      parseFuzzArg "  ?  " @?= Just Nothing
  , testCase "concrete uint wrapped in Just" $
      parseFuzzArg "42" @?= Just (Just (uint 42))
  , testCase "concrete bool wrapped in Just" $
      parseFuzzArg "true" @?= Just (Just (AbiBool True))
  , testCase "non-numeric garbage rejected" $
      assertBool "expected Nothing" (isNothing (parseFuzzArg "abc"))
  ]

fuzzCallTests :: TestTree
fuzzCallTests = testGroup "parseFuzzCall"
  [ testCase "no args" $
      parseFuzzCall "foo()" @?= Just ("foo", [])
  , testCase "two concrete uints" $
      parseFuzzCall "foo(1, 2)" @?=
        Just ("foo", [Just (uint 1), Just (uint 2)])
  , testCase "mix of concrete and fuzz args" $
      parseFuzzCall "transfer(?, 42)" @?=
        Just ("transfer", [Nothing, Just (uint 42)])
  , testCase "array argument alongside fuzz arg" $
      parseFuzzCall "bar([1,2], ?)" @?=
        Just ("bar", [Just (uintArray [1, 2]), Nothing])
  , testCase "missing opening paren rejected" $
      assertBool "expected Nothing" (isNothing (parseFuzzCall "foo)"))
  , testCase "garbage argument fails the whole call" $
      assertBool "expected Nothing" (isNothing (parseFuzzCall "foo(abc)"))
  ]

fuzzSequenceTests :: TestTree
fuzzSequenceTests = testGroup "parseFuzzSequence"
  [ testCase "single call" $
      parseFuzzSequence "foo()" @?= Just [("foo", [])]
  , testCase "two calls separated by ';'" $
      parseFuzzSequence "foo(1); bar(?)" @?=
        Just [ ("foo", [Just (uint 1)])
             , ("bar", [Nothing])
             ]
  , testCase "failure in any call fails the whole sequence" $
      assertBool "expected Nothing"
        (isNothing (parseFuzzSequence "foo(1); bad)"))
  ]

callTests :: TestTree
callTests = testGroup "parseCall"
  [ testCase "no args" $
      parseCall "foo()" @?= Just ("foo", [])
  , testCase "concrete args of mixed primitive types" $
      parseCall "foo(1, true, 0x10)" @?=
        Just ("foo", [uint 1, AbiBool True, addr 0x10])
  , testCase "'?' is rejected (concrete-only parser)" $
      assertBool "expected Nothing" (isNothing (parseCall "foo(?)"))
  , testCase "missing opening paren rejected" $
      assertBool "expected Nothing" (isNothing (parseCall "foo)"))
  ]

splitArgsTests :: TestTree
splitArgsTests = testGroup "splitArgs"
  [ testCase "flat comma-separated list" $
      splitArgs "1,2,3" @?= ["1", "2", "3"]
  , testCase "respects single-level bracket nesting" $
      splitArgs "[1,2],3" @?= ["[1,2]", "3"]
  , testCase "respects deep bracket nesting" $
      splitArgs "[[1,2],3],4" @?= ["[[1,2],3]", "4"]
  , testCase "single token (no commas)" $
      splitArgs "a" @?= ["a"]
  ]
