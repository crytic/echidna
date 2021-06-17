module Tests.Encoding (encodingJSONTests) where

import Prelude hiding (Word)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Gen, (===), property, testProperty, resize)

import Data.Aeson (encode, decode)
import Data.Text (pack)
import Echidna.Types.Tx (TxCall(..), Tx(..))
import EVM.Types (Addr, Word)

instance Arbitrary Addr where
  arbitrary = fromInteger <$> arbitrary

instance Arbitrary Word where
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
