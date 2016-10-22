{-# LANGUAGE OverloadedStrings #-}
module Network.RefractionSpec (spec) where

import Data.Either
import qualified Data.Serialize as S
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import qualified Network.Refraction as R
import Test.Hspec

spec :: Spec
spec = do
    describe "isValidPrivateKey" $ do
        it "returns true for valid privaty keys" $ do
            R.isValidPrivateKey "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ" `shouldBe` True
        it "returns fales for invalid private keys" $ do
            R.isValidPrivateKey "foo" `shouldBe` False
    describe "isValidAddress" $ do
        it "returns true for valid address" $ do
            R.isValidAddress "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2" `shouldBe` True
        it "returns false for invalid address" $ do
            R.isValidAddress "foo" `shouldBe` False
