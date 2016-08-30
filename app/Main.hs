{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import Control.Applicative
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Refraction as R

data RefractionConfig = RefractionConfig {
    bitcoin :: BitcoinConfig
} deriving Show
data BitcoinConfig = BitcoinConfig {
    network :: Text
} deriving Show
instance FromJSON RefractionConfig where
    parseJSON (Object m) = RefractionConfig <$> m .: "bitcoin"
    parseJSON x = fail ("not an object: " ++ show x)
instance FromJSON BitcoinConfig where
    parseJSON (Object m) = BitcoinConfig <$> m .: "network"
    parseJSON x = fail ("not an object: " ++ show x)

readConfig :: IO RefractionConfig
readConfig =
    either (error . show) id <$> decodeFileEither ".refraction.yaml"

main :: IO ()
main = do
    let isBob = True
    config <- readConfig
    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine
    putStr "Enter destination address (Base58 encoded): "
    hFlush stdout
    pubkey <- T.getLine
    R.refract (network (bitcoin config)) isBob prvkey pubkey
