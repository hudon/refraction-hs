{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module FairExchange
    ( fairExchange
    ) where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (guard, liftM)
import Control.Monad.Random (evalRandIO)
import Crypto.Random.DRBG (CtrDRBG, genBytes, newGenIO)
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Serialize as S
import Discover (Location)
import GHC.Generics
import Network.Haskoin.Crypto (derivePubKey, doubleHash256, getEntropy, genPrvKey, Hash256, PrvKey, PubKey, withSource)
import PeerToPeer (Msg, unsecureSend)
import System.Random.Shuffle (shuffleM)

type KeyPair = (PrvKey, PubKey)
type Secret = BL.ByteString
type SecretSum = BL.ByteString

fairExchange :: Bool -> Bool -> Chan Msg -> Location -> Location -> IO ()
fairExchange isBob isAlice chan _ theirLocation = do
    setupSecrets isBob chan theirLocation
    putStrLn "fairExchange called"

data KeysMessage = KeysMessage {
      key1 :: Maybe PubKey
    , key2 :: Maybe PubKey
    , hex1 :: [Text] -- ^ hex encoded
    , hex2 :: Maybe [Text] -- ^ hex encoded, alice doesn't need this field
} deriving (Generic)

instance ToJSON KeysMessage

instance FromJSON KeysMessage

setupSecrets :: Bool -> Chan Msg -> Location -> IO ()
setupSecrets isBob chan _ = do
    keypair1 <- makeKeyPair
    keypair2 <- makeKeyPair
    let n = 20
    let m = 5
    mySecrets <- genSecrets n
    let f = if isBob then setupBobSecrets else setupAliceSecrets
    f chan keypair1 keypair2 n m mySecrets
  where
    makeKeyPair = genKey >>= \p -> return (p, derivePubKey p)

setupBobSecrets :: Chan Msg -> KeyPair -> KeyPair -> Int -> Int -> [Secret] -> IO ()
setupBobSecrets chan (prvkey1, pubkey1) (prvkey2, pubkey2) n _ mySecrets = do
    aliceKeys <- liftM (fromMaybe undefined . decode) $ readChan chan
    let aliceSecrets = map hexTextToBs $ hex1 aliceKeys
    let bobHashes = map (bsToHexText . S.encodeLazy . doubleHash256 . BL.toStrict) mySecrets
    let sums = zipWith sumSecrets aliceSecrets mySecrets
    let sumHashes = map (bsToHexText . S.encodeLazy . doubleHash256 . BL.toStrict) sums
    unsecureSend True . encode $ KeysMessage (Just pubkey1) (Just pubkey2) bobHashes (Just sumHashes)
    indices <- liftM (fromMaybe undefined . decode) $ readChan chan
    unsecureSend True . encodeSecrets $ map ((!!) mySecrets) indices
    _ <- readChan chan
    --return (mySecrets, aliceSecrets, bobHashes, sumHashes, keypairs, aliceKeys)
    return ()

setupAliceSecrets :: Chan Msg -> KeyPair -> KeyPair -> Int -> Int -> [Secret] -> IO ()
setupAliceSecrets chan (prvkey1, pubkey1) (prvkey2, pubkey2) m n mySecrets = do
    unsecureSend False . encode $ KeysMessage (Just pubkey1) (Just pubkey2) (map (decodeUtf8 . encodeHex) mySecrets) Nothing
    bobKeys <- liftM (fromMaybe undefined . decode) $ readChan chan
    indices <- evalRandIO $ liftM (take m) $ shuffleM [1..n]
    unsecureSend False (encode indices)
    bobSecrets <- liftM decodeSecrets (readChan chan)
    verifySecrets bobSecrets mySecrets (map (either undefined id . S.decodeLazy . hexTextToBs) $ hex1 bobKeys) (map (either undefined id . S.decodeLazy . hexTextToBs) . fromMaybe undefined $ hex2 bobKeys)
    unsecureSend False "ok"
    --return (mySecrets, bobHashes, sumHashes bobKeys)
    return ()

sumSecrets :: Secret -> Secret -> SecretSum
sumSecrets x y = S.encodeLazy $ (int x) + (int y)
  where
    int :: Secret -> Integer
    int = either undefined id . S.decodeLazy

encodeSecrets :: [Secret] -> Msg
encodeSecrets = encode . map bsToHexText

decodeSecrets :: Msg -> [Secret]
decodeSecrets = map hexTextToBs . fromMaybe undefined . decode

bsToHexText :: BL.ByteString -> Text
bsToHexText = decodeUtf8 . encodeHex

hexTextToBs :: Text -> BL.ByteString
hexTextToBs = fromMaybe undefined . decodeHex . encodeUtf8

-- | Decode hexadecimal lazy 'ByteString'. This function can fail if the string
-- contains invalid hexadecimal (0-9, a-f, A-F) characters
decodeHex :: BL.ByteString -> Maybe BL.ByteString
decodeHex bs =
    let (x, b) = B16.decode bs
    in guard (b == BL.empty) >> return x

encodeHex = B16.encode

verifySecrets :: [Secret] -> [Secret] -> [Hash256] -> [Hash256] -> IO Bool
verifySecrets bSecrets aSecrets bHashes sumHashes = return True

genKey :: IO PrvKey
genKey = withSource getEntropy genPrvKey

genSecrets :: Int -> IO [Secret]
genSecrets n
  | n > 0 = do
    secret <- genSecret
    secrets <- genSecrets (n - 1)
    return $ secret : secrets
  | otherwise = return []
  where
    genSecret = do
        g <- newGenIO :: IO CtrDRBG
        let size = 256
        case genBytes size g of
            Left err -> error $ show err
            Right (result, g2) -> return $ BL.fromStrict result
