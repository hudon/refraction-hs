{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.Hspec as H
import Test.Tasty.QuickCheck as QC

import qualified Network.Refraction as R
import qualified Spec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)" []

unitTests = testGroup "Unit tests"
  [ testCase "Invalid Private Key" $
      R.isValidPrivateKey "foo" @?= False

  , testCase "Valid Private Key" $
      R.isValidPrivateKey "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" @?= True

  , testCase "Invalid Address" $
      R.isValidAddress "foo" @?= False

  , testCase "Valid Address" $
      R.isValidAddress "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2" @?= True
  ]
