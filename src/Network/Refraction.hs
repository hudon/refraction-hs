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
import Network (PortNumber)
import Network.Haskoin.Constants (switchToTestnet3)
import Network.Haskoin.Transaction (OutPoint(..))
import qualified Network.Haskoin.Crypto as C
import Network.Refraction.Discover (discover)
import Network.Refraction.FairExchange (fairExchange)
import Network.Refraction.Generator (UTXO(..))
import qualified Network.Refraction.PeerToPeer as P2P
import Network.Refraction.Tor (makeHiddenService)

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

startRound :: Bool -> Bool -> C.PrvKey -> C.Address -> IO ()
startRound isBob isAlice prv addr = do
    let port = if isBob then 4242 else 4243 :: PortNumber
    chan <- P2P.startServer port
    makeHiddenService port $ \myLocation -> do
        putStrLn $ "hidden service location: " ++ show myLocation
        theirLocation <- discover chan myLocation isBob isAlice prv
        putStrLn "discover done!"
        --fairExchange isBob isAlice chan myLocation theirLocation

refract :: RefractionConfig -> Bool -> Bool -> Bool -> Text -> Text -> IO ()
refract config isBob isAlice ignoreValidation prv addr = do
    let btcNetwork = network . bitcoin $ config
    TI.putStrLn $ append "INFO: Starting refraction on " btcNetwork
    when (btcNetwork == "testnet3") switchToTestnet3
    case () of
      _ | not (ignoreValidation || isValidPrivateKey prv) -> handleBadPrvkey prv
        | not (ignoreValidation || isValidAddress addr) -> handleBadAddress addr
        | otherwise -> startRound isBob isAlice p a
  where
    p = fromMaybe undefined . C.fromWif $ TE.encodeUtf8 prv
    a = fromMaybe undefined . C.base58ToAddr $ TE.encodeUtf8 addr
