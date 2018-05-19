module Main where

import Echidna.ABI (genAbiValue, mutateValue)
import EVM.ABI (abiValueType)
import Hedgehog

checkAbi :: Property
checkAbi = property $ do 
  val <- forAll genAbiValue
  val' <- forAll $ mutateValue val
  abiValueType val === abiValueType val'

main :: IO Bool
main = check checkAbi

