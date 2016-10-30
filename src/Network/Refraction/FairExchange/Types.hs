{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Refraction.FairExchange.Types
    ( KeyPair
    , Secret
    , numSecrets
    , numChallenges
    , BobKeyMessage(..)
    , AliceKeysMessage(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import Network.Haskoin.Crypto

type KeyPair = (PrvKey, PubKey)
type Secret = Integer

numSecrets = 4 :: Int
numChallenges = 2 :: Int

data BobKeyMessage = BobKeyMessage {
      bKey1 :: PubKey
    , bKey2 :: PubKey
    , bHashes :: [Text] -- ^ text for the hex-encoded bytestring
    , bSumHashes :: [Text] -- ^ text for the hex-encoded bytestring
} deriving (Generic)

instance ToJSON BobKeyMessage

instance FromJSON BobKeyMessage

data AliceKeysMessage = AliceKeysMessage {
      aKey1 :: PubKey
    , aKey2 :: PubKey
    , aSecrets :: [Secret]
} deriving (Generic)

instance ToJSON AliceKeysMessage

instance FromJSON AliceKeysMessage
