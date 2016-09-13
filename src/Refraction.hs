{-# LANGUAGE OverloadedStrings #-}
module Refraction
    ( isValidPrivateKey
    , isValidAddress
    , refract
    , RefractionConfig
    ) where

import qualified PeerToPeer as P2P
import Data.Text (Text, append)
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as TE
import Data.Yaml
import qualified Network.Haskoin.Crypto as C

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

refract :: RefractionConfig -> Bool -> Bool -> Text -> Text -> IO ()
refract config isBob ignoreValidation prv addr = do
    TI.putStrLn $ append "INFO: Starting refraction on " (network (bitcoin config))
    case () of
      _ | not (ignoreValidation || isValidPrivateKey prv) -> handleBadPrvkey prv
        | not (ignoreValidation || isValidAddress addr) -> handleBadAddress addr
        | isBob -> P2P.startServer
        | otherwise -> P2P.startClient
