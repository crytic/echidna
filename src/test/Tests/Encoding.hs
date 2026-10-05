module Tests.Encoding (encodingJSONTests) where

import Data.Aeson (Value, encode, decode, toJSON)
import Data.ByteString qualified as BS
import Data.Text (pack)
import Data.Vector qualified as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, (===), property, testProperty, resize)

import EVM.ABI (AbiType(..), AbiValue(..))
import EVM.Types (Addr, W256)

import Echidna.Output.JSON (abiValueToJSON)
import Echidna.Types.Tx (TxCall(..), Tx(..))

instance Arbitrary Addr where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary W256 where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary TxCall where
  arbitrary = do
    s <- arbitrary
    cs <- resize 32 arbitrary
    return $ SolCall (pack s, cs)

instance Arbitrary Tx where
  arbitrary = Tx <$> a <*> a <*> a <*> a <*> a <*> a <*> a
    where a :: Arbitrary a => Gen a
          a = arbitrary

encodingJSONTests :: TestTree
encodingJSONTests =
  testGroup "Tx JSON encoding"
    [ testProperty "decode . encode = id" $ property $ do
        t <- arbitrary :: Gen Tx
        return $ decode (encode t) === Just t
    , abiValueJSONTests
    ]

-- | Call arguments must be structured JSON, not Haskell 'show' output.
abiValueJSONTests :: TestTree
abiValueJSONTests =
  testGroup "Tx argument JSON encoding"
    [ testCase "scalars keep their textual form" $ do
        abiValueToJSON (AbiUInt 256 42) @?= toJSON ("42" :: String)
        abiValueToJSON (AbiBool True) @?= toJSON ("true" :: String)
        abiValueToJSON (AbiAddress 0) @?= toJSON ("0x0" :: String)
    , testCase "string is a plain JSON string" $
        abiValueToJSON (AbiString "h\195\169llo \"x\"") @?= toJSON ("h\233llo \"x\"" :: String)
    , testCase "string that is not UTF-8 falls back to hex" $
        abiValueToJSON (AbiString (BS.pack [0xff, 0x00])) @?= toJSON ("0xff00" :: String)
    , testCase "bytes is 0x-hex, with no Haskell escapes" $
        abiValueToJSON (AbiBytesDynamic (BS.pack [0, 127, 232, 38])) @?= toJSON ("0x007fe826" :: String)
    , testCase "bytesN is 0x-hex" $
        abiValueToJSON (AbiBytes 2 (BS.pack [0xab, 0x32])) @?= toJSON ("0xab32" :: String)
    , testCase "bytes32[] is a JSON array of hex strings" $
        abiValueToJSON (AbiArrayDynamic (AbiBytesType 32)
                         (V.fromList [AbiBytes 32 (BS.replicate 31 0 <> BS.pack [0xab])]))
          @?= toJSON [toJSON ("0x" <> replicate 62 '0' <> "ab")]
    , testCase "tuple is a JSON array, encoded recursively" $
        abiValueToJSON (AbiTuple (V.fromList
          [ AbiString "a"
          , AbiArray 2 AbiBoolType (V.fromList [AbiBool True, AbiBool False])
          , AbiBytesDynamic (BS.pack [1])
          ]))
          @?= (toJSON [ toJSON ("a" :: String)
                      , toJSON [toJSON ("true" :: String), toJSON ("false" :: String)]
                      , toJSON ("0x01" :: String) ] :: Value)
    ]
