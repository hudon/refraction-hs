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
import Data.Word
import Discover (Location)
import GHC.Generics
import Network.Haskoin.Crypto (derivePubKey, doubleHash256, getEntropy, genPrvKey, Hash256, PrvKey, PubKey, withSource)
import PeerToPeer (Msg, unsecureSend)
import System.Random.Shuffle (shuffleM)

type KeyPair = (PrvKey, PubKey)
-- Because we'll be summing up these secrets, it's important to note that these bytes are
-- in little endian. The least significant byte comes first.
type Secret = BL.ByteString

fairExchange :: Bool -> Bool -> Chan Msg -> Location -> Location -> IO ()
fairExchange isBob isAlice chan _ theirLocation = do
    setupSecrets isBob chan theirLocation
    putStrLn "fairExchange called"

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

data KeysMessage = KeysMessage {
      key1 :: PubKey
    , key2 :: PubKey
    , hex1 :: [Text] -- ^ hex encoded
    , hex2 :: Maybe [Text] -- ^ hex encoded, alice doesn't need this field
} deriving (Generic)

instance ToJSON KeysMessage

instance FromJSON KeysMessage

setupBobSecrets :: Chan Msg -> KeyPair -> KeyPair -> Int -> Int -> [Secret] -> IO ()
setupBobSecrets chan (prvkey1, pubkey1) (prvkey2, pubkey2) n _ mySecrets = do
    aliceKeys <- liftM (fromMaybe undefined . decode) $ readChan chan
    let aliceSecrets = map hexTextToBs $ hex1 aliceKeys
    let bobHashes = map hashAndEncode mySecrets
    let sums = zipWith sumSecrets aliceSecrets mySecrets
    let sumHashes = map hashAndEncode sums
    send $ KeysMessage pubkey1 pubkey2 bobHashes (Just sumHashes)
    indices <- liftM (fromMaybe undefined . decode) $ readChan chan
    send $ map (bsToHexText . (!!) mySecrets) indices
    return ()
  where
    hashAndEncode = bsToHexText . S.encodeLazy . doubleHash256 . BL.toStrict
    send x = unsecureSend True (encode x)
    sumSecrets bs1 bs2 = BL.pack $ byteSum (BL.unpack bs1) (BL.unpack bs2)

byteSum :: [Word8] -> [Word8] -> [Word8]
byteSum = byteSum' 0
  where
    byteSum' 0 [] [] = []
    byteSum' 1 [] [] = [1]
    byteSum' carry (x:xs) (y:ys) = let v = x + y + carry in v : byteSum' (if v < x || v < y then 1 else 0) xs ys

setupAliceSecrets :: Chan Msg -> KeyPair -> KeyPair -> Int -> Int -> [Secret] -> IO ()
setupAliceSecrets chan (prvkey1, pubkey1) (prvkey2, pubkey2) m n mySecrets = do
    send $ KeysMessage pubkey1 pubkey2 (map bsToHexText mySecrets) Nothing
    bobKeys <- liftM (fromMaybe undefined . decode) $ readChan chan
    indices <- evalRandIO $ liftM (take m) $ shuffleM [1..n]
    send indices
    bobSecrets <- liftM decodeSecrets (readChan chan)
    verifySecrets bobSecrets mySecrets (map decodeHashes $ hex1 bobKeys) (map decodeHashes . fromMaybe undefined $ hex2 bobKeys)
    return ()
  where
    decodeHashes = either undefined id . S.decodeLazy . hexTextToBs
    send x = unsecureSend False (encode x)
    decodeSecrets = map hexTextToBs . fromMaybe undefined . decode

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
