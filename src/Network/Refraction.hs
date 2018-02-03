{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction
    ( isValidPrivateKey
    , isValidAddress
    , refract
    , RefractionConfig
    ) where

import Control.Monad (when)
import Data.Maybe
import Data.Text (Text, append)
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as TE
import Data.Yaml
import Network.Haskoin.Constants (setTestnet)
import Network.Haskoin.Transaction (OutPoint(..))
import qualified Network.Haskoin.Crypto as C

import Network.Refraction.BitcoinUtils
import Network.Refraction.RoundManager (prepareRounds)

-- There may be a simpler way to express this, but this is our config file type declaration
data RefractionConfig = RefractionConfig { bitcoin :: BitcoinConfig } deriving Show

data BitcoinConfig = BitcoinConfig { network :: Text, insightURL :: Text } deriving Show

instance FromJSON RefractionConfig where
    parseJSON (Object m) = RefractionConfig <$> m .: "bitcoin"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON BitcoinConfig where
    parseJSON (Object m) = BitcoinConfig <$>
                           m .: "network" <*>
                           m .: "insightURL"
    parseJSON x = fail ("not an object: " ++ show x)

isValidPrivateKey :: Text -> Bool
isValidPrivateKey prv =
    case C.fromWif (TE.encodeUtf8 prv) of
        Nothing -> False
        Just _ -> True

handleBadPrvkey :: Text -> IO ()
handleBadPrvkey prv = putStrLn "ERROR: private key is not valid"

isValidAddress :: Text -> Bool
isValidAddress addr = case C.base58ToAddr (TE.encodeUtf8 addr) of
    Nothing -> False
    Just _ -> True

handleBadAddress :: Text -> IO()
handleBadAddress addr = putStrLn "ERROR: address is not valid"

refract :: RefractionConfig -> Bool -> Bool -> Bool -> Text -> Text -> Text -> IO ()
refract config isBob isAlice ignoreValidation prv endAddr refundAddr = do
    let btcNetwork = network . bitcoin $ config
    TI.putStrLn $ append "INFO: Starting refraction on " btcNetwork
    when (btcNetwork == "testnet3") setTestnet
    case () of
      _ | not (ignoreValidation || isValidPrivateKey prv) -> handleBadPrvkey prv
        | not (ignoreValidation || isValidAddress endAddr) -> handleBadAddress endAddr
      -- | not (ignoreValidation || isValidAddress startAddr) -> handleBadAddress startAddr
        | not (ignoreValidation || isValidAddress refundAddr) -> handleBadAddress refundAddr
        | otherwise -> prepareRounds isBob isAlice p e r
  where
    --s = deserialize startAddr -- not used. we'll use the private key for now
    e = deserialize endAddr
    r = deserialize refundAddr
    p = fromMaybe undefined . C.fromWif $ TE.encodeUtf8 prv
    deserialize = fromMaybe undefined . C.base58ToAddr . TE.encodeUtf8
