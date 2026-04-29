module Tests.Dict (dictTests) where

import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import EVM.ABI (AbiType(AbiUIntType), AbiValue(AbiUInt))

import Echidna.ABI
import Echidna.Types.Signature (SolCall, SolSignature)

dictTests :: TestTree
dictTests =
  testGroup "Dictionary tests"
    [ boundedSetTests
    , genDictTests
    ]

boundedSetTests :: TestTree
boundedSetTests =
  testGroup "BoundedSet"
    [ testCase "evicts oldest values by FIFO order" $ do
        let bounded = boundedInsertSet 2 Set.empty (Set.fromList [1 :: Int, 2, 3]) emptyBoundedSet
        assertEqual "" [2, 3] (boundedElems bounded)
    , testCase "duplicate insert does not refresh position" $ do
        let bounded = boundedInsertSet 2 Set.empty (Set.fromList [1 :: Int, 2]) emptyBoundedSet
            bounded' = boundedInsert 2 Set.empty 1 bounded
            bounded'' = boundedInsert 2 Set.empty 3 bounded'
        assertEqual "" [2, 3] (boundedElems bounded'')
    , testCase "limit zero stores nothing" $ do
        let bounded = boundedInsert 0 Set.empty (1 :: Int) emptyBoundedSet
        assertEqual "" [] (boundedElems bounded)
    , testCase "static values are not inserted dynamically" $ do
        let bounded = boundedInsert 2 (Set.singleton (1 :: Int)) 1 emptyBoundedSet
        assertEqual "" [] (boundedElems bounded)
    ]

genDictTests :: TestTree
genDictTests =
  testGroup "GenDict"
    [ testCase "dynamic constants are capped per ABI type and static constants survive" $ do
        let dict = mkGenDict 1 (Set.singleton abi1) Set.empty 0 (const Nothing) 2 10 10 []
            dict' = addDynamicConstants (Map.singleton abiUInt (Set.fromList [abi1, abi2, abi3, abi4])) dict
        assertBool "static constant should remain static" $
          abi1 `Set.member` Map.findWithDefault Set.empty abiUInt dict'.staticConstants
        assertEqual "" [abi3, abi4] $
          boundedElems (Map.findWithDefault emptyBoundedSet abiUInt dict'.dynamicConstants)
    , testCase "dynamic int dictionary is capped separately from static values" $ do
        let dict = mkGenDict 1 (Set.singleton abi1) Set.empty 0 (const Nothing) 10 2 10 []
            dict' = addDynamicConstants (Map.singleton abiUInt (Set.fromList [abi1, abi2, abi3, abi4])) dict
        assertBool "static numeric value should remain static" $
          1 `Set.member` dict'.staticDictValues
        assertEqual "" [3, 4] (boundedElems dict'.dynamicDictValues)
    , testCase "dynamic whole calls are capped per signature" $ do
        let call1 = solCall "f" [abi1]
            call2 = solCall "f" [abi2]
            sig = solSig "f" [abiUInt]
            dict = mkGenDict 1 Set.empty Set.empty 0 (const Nothing) 10 10 1 []
            dict' = gaddCalls (Set.fromList [call1, call2]) dict
        assertEqual "" [call2] $
          boundedElems (Map.findWithDefault emptyBoundedSet sig dict'.dynamicWholeCalls)
    ]

boundedElems :: BoundedSet a -> [a]
boundedElems = Foldable.toList . (.boundedOrder)

abiUInt :: AbiType
abiUInt = AbiUIntType 256

abi1, abi2, abi3, abi4 :: AbiValue
abi1 = AbiUInt 256 1
abi2 = AbiUInt 256 2
abi3 = AbiUInt 256 3
abi4 = AbiUInt 256 4

solCall :: Text -> [AbiValue] -> SolCall
solCall name values = (name, values)

solSig :: Text -> [AbiType] -> SolSignature
solSig name types = (name, types)
