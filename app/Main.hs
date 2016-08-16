{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory (doesFileExist)
import System.IO
import Control.Applicative
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Refraction as R


configFilename = ".refraction.yaml"

-- There may be a simpler way to express this, but this is our config file type declaration
data RefractionConfig = RefractionConfig { bitcoin :: BitcoinConfig } deriving Show
data BitcoinConfig = BitcoinConfig { network :: Text } deriving Show
instance FromJSON RefractionConfig where
    parseJSON (Object m) = RefractionConfig <$> m .: "bitcoin"
    parseJSON x = fail ("not an object: " ++ show x)
instance FromJSON BitcoinConfig where
    parseJSON (Object m) = BitcoinConfig <$> m .: "network"
    parseJSON x = fail ("not an object: " ++ show x)

readConfig :: IO RefractionConfig
readConfig = either (error . show) id <$> decodeFileEither configFilename

touchConfig :: IO ()
touchConfig = doesFileExist configFilename >>= writeIfFalse
  where writeIfFalse False = writeFile configFilename "bitcoin:\n  network: testnet3"
        writeIfFalse _ = return ()

main :: IO ()
main = do
    touchConfig
    config <- readConfig
    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine
    putStr "Enter destination address (Base58 encoded): "
    hFlush stdout
    pubkey <- T.getLine
    R.refract (network (bitcoin config)) prvkey pubkey
