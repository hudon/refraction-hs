{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Data.Yaml
import Control.Applicative -- <$>, <*>
import Lib

data Config = Config { electrumPath :: String
                     , electrumWallet :: String
                     } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = Config <$>
                           (v .: "electrum" >>= (.: "path")) <*>
                           (v .: "electrum" >>= (.: "wallet"))
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Config from YAML/JSON"

main :: IO ()
main = do
    file <- decodeFile "app/.template.yml" :: IO (Maybe Config)
    let path = electrumPath $ fromJust file
        wallet = electrumWallet $ fromJust file
    putStrLn path
