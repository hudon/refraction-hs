{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction
    ( isValidPrivateKey
    , isValidAddress
    , refract
    , RefractionConfig
    ) where

import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Yaml
import Network.Haskoin.Constants (setTestnet)
import Network.Haskoin.Transaction (OutPoint(..))
import qualified Network.Haskoin.Crypto as C

import Network.Refraction.BitcoinUtils
import Network.Refraction.RoundManager (prepareRounds)

-- There may be a simpler way to express this, but this is our config file type declaration
data RefractionConfig = RefractionConfig { bitcoin :: BitcoinConfig } deriving Show

data BitcoinConfig = BitcoinConfig { network :: String, insightURL :: String } deriving Show

instance FromJSON RefractionConfig where
    parseJSON (Object m) = RefractionConfig <$> m .: "bitcoin"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON BitcoinConfig where
    parseJSON (Object m) = BitcoinConfig <$>
                           m .: "network" <*>
                           m .: "insightURL"
    parseJSON x = fail ("not an object: " ++ show x)

encodeUtf8 :: String -> B.ByteString
encodeUtf8 = TE.encodeUtf8 . T.pack

isValidPrivateKey :: String -> Bool
isValidPrivateKey prv =
    case C.fromWif (encodeUtf8 prv) of
        Nothing -> False
        Just _ -> True

handleBadPrvkey :: String -> IO ()
handleBadPrvkey prv = putStrLn "ERROR: private key is not valid"

isValidAddress :: String -> Bool
isValidAddress addr = case C.base58ToAddr (encodeUtf8 addr) of
    Nothing -> False
    Just _ -> True

handleBadAddress :: String -> IO()
handleBadAddress addr = putStrLn "ERROR: address is not valid"

refract :: RefractionConfig -> Bool -> Bool -> String -> String -> String -> IO ()
refract config isBob isAlice prv endAddr refundAddr = do
    let btcNetwork = network . bitcoin $ config
    putStrLn $ "INFO: Starting refraction on " ++ btcNetwork
    when (btcNetwork == "testnet3") setTestnet
    case () of
      _ | not (isValidPrivateKey prv) -> handleBadPrvkey prv
        | not (isValidAddress endAddr) -> handleBadAddress endAddr
        | not (isValidAddress refundAddr) -> handleBadAddress refundAddr
        | otherwise -> prepareRounds isBob isAlice p e r
  where
    --s = deserialize startAddr -- not used. we'll use the private key for now
    e = deserialize endAddr
    r = deserialize refundAddr
    p = fromMaybe undefined . C.fromWif $ encodeUtf8 prv
    deserialize = fromMaybe undefined . C.base58ToAddr . encodeUtf8
