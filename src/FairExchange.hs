{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module FairExchange
    ( fairExchange
    ) where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (guard, liftM)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Serialize as S
import Data.Word
import Discover (Location)
import GHC.Generics
import Network.Haskoin.Crypto (derivePubKey, doubleHash256, getEntropy, genPrvKey, Hash256, PrvKey, PubKey, withSource)
import PeerToPeer (Msg, unsecureSend)

type KeyPair = (PrvKey, PubKey)
type Secret = Integer

fairExchange :: Bool -> Bool -> Chan Msg -> Location -> Location -> IO ()
fairExchange isBob isAlice chan _ theirLocation = do
    setupSecrets isBob chan theirLocation
    putStrLn "fairExchange called"

setupSecrets :: Bool -> Chan Msg -> Location -> IO ()
setupSecrets isBob chan _ = do
    keypair1 <- makeKeyPair
    keypair2 <- makeKeyPair
    let n = 20
        m = 5
        f = if isBob then setupBobSecrets else setupAliceSecrets
    mySecrets <- genSecrets n
    f chan keypair1 keypair2 n m mySecrets
  where
    makeKeyPair = genKey >>= \p -> return (p, derivePubKey p)
    genKey = withSource getEntropy genPrvKey

genSecrets :: Int -> IO [Integer]
genSecrets n = do
    -- TODO: just use 1 generator for this module? instead of using a new one to shuffle later
    g <- newGenIO :: IO CtrDRBG
    return . take n $ crandomRs (-2^255, 2^255-1) g

data BobKeyMessage = BobKeyMessage {
      bKey1 :: PubKey
    , bKey2 :: PubKey
    , bHashes :: [Text] -- ^ text for the hex-encoded bytestring
    , bSumHashes :: [Text] -- ^ text for the hex-encoded bytestring
} deriving (Generic)

instance ToJSON BobKeyMessage

instance FromJSON BobKeyMessage

setupBobSecrets :: Chan Msg -> KeyPair -> KeyPair -> Int -> Int -> [Secret] -> IO ()
setupBobSecrets chan (prvkey1, pubkey1) (prvkey2, pubkey2) n _ mySecrets = do
    aliceKeys <- liftM (fromMaybe undefined . decode) $ readChan chan
    let aliceSecrets = aSecrets aliceKeys
        bobHashes = map hashAndEncode mySecrets
        sums = zipWith (+) aliceSecrets mySecrets
        sumHashes = map hashAndEncode sums
    send $ BobKeyMessage pubkey1 pubkey2 bobHashes sumHashes
    indices <- liftM (fromMaybe undefined . decode) $ readChan chan
    send $ map ((!!) mySecrets) indices
    return ()
  where
    hashAndEncode = bsToHexText . S.encodeLazy . doubleHash256 . S.encode
    send x = unsecureSend True (encode x)

data AliceKeysMessage = AliceKeysMessage {
      aKey1 :: PubKey
    , aKey2 :: PubKey
    , aSecrets :: [Secret]
} deriving (Generic)

instance ToJSON AliceKeysMessage

instance FromJSON AliceKeysMessage

setupAliceSecrets :: Chan Msg -> KeyPair -> KeyPair -> Int -> Int -> [Secret] -> IO ()
setupAliceSecrets chan (prvkey1, pubkey1) (prvkey2, pubkey2) m n mySecrets = do
    send $ AliceKeysMessage pubkey1 pubkey2 mySecrets
    bobKeys <- liftM (fromMaybe undefined . decode) $ readChan chan
    indices <- liftM (take m) $ shuffle [1..n]
    send indices
    bobSecrets <- liftM decodeSecrets (readChan chan)
    verifySecrets bobSecrets mySecrets (map decodeHashes $ bHashes bobKeys) (map decodeHashes $ bSumHashes bobKeys)
    return ()
  where
    decodeHashes = either undefined id . S.decodeLazy . hexTextToBs
    send x = unsecureSend False (encode x)
    decodeSecrets = fromMaybe undefined . decode

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    g <- newGenIO :: IO CtrDRBG
    let (ys, (v:zs)) = splitAt (head $ crandomRs (0, length xs - 1) g) xs
    liftM ((:) v) $ shuffle (ys ++ zs)

bsToHexText :: BL.ByteString -> Text
bsToHexText = decodeUtf8 . B16.encode

hexTextToBs :: Text -> BL.ByteString
hexTextToBs = fromMaybe undefined . decodeHex . encodeUtf8
  where
    decodeHex bs =
        let (x, b) = B16.decode bs
        in guard (b == BL.empty) >> return x

verifySecrets :: [Secret] -> [Secret] -> [Hash256] -> [Hash256] -> IO Bool
verifySecrets bSecrets aSecrets bHashes sumHashes = return True
