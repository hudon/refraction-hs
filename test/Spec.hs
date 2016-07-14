{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import qualified Refraction as R

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)" []
qcProps = testGroup "(checked by QuickCheck)" []

unitTests = testGroup "Unit tests"
  [ testCase "Invalid Private Key" $
      R.isValidPrivateKey "foo" @?= False

  , testCase "Valid Private Key" $
      R.isValidPrivateKey "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" @?= True
  ]
