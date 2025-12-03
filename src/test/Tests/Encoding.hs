module Tests.Encoding (encodingJSONTests) where

import Data.Aeson (encode, decode)
import Data.Text (pack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, (===), property, testProperty, resize)

import EVM.Types (Addr, W256)

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
    ]
