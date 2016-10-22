module Network.Refraction.FairExchange.Alice
    ( runAlice
    ) where

import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (guard, liftM)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Network.Haskoin.Crypto
import Network.Refraction.Discover (Location)
import Network.Refraction.FairExchange.Types
import Network.Refraction.PeerToPeer (Msg, sendMessage)
import Network.Refraction.Tor(secureConnect)

runAlice :: Chan Msg -> KeyPair -> KeyPair -> [Secret] -> Location -> IO ()
runAlice chan keypair refundKeypair mySecrets theirLocation = do
    putStrLn "Running Alice fair exchange protocol..."
    setupAliceSecrets chan keypair refundKeypair mySecrets theirLocation
    aliceCommit keypair theirLocation
    aliceClaim

setupAliceSecrets :: Chan Msg -> KeyPair -> KeyPair -> [Secret] -> Location -> IO ()
setupAliceSecrets chan (_, aPub) (_, refundPub) mySecrets theirLocation = do
    send $ AliceKeysMessage aPub refundPub mySecrets
    bobKeys <- liftM (fromMaybe undefined . A.decode) $ readChan chan
    indices <- liftM (take numChallenges) $ shuffle [1..numSecrets]
    send indices
    bobSecrets <- liftM decodeSecrets (readChan chan)
    verifySecrets bobSecrets mySecrets (map decodeHashes $ bHashes bobKeys) (map decodeHashes $ bSumHashes bobKeys)
    return ()
  where
    decodeHashes = either undefined id . S.decodeLazy . hexTextToBs
    send x = secureConnect theirLocation $ sendMessage (A.encode x)
    decodeSecrets = fromMaybe undefined . A.decode

verifySecrets :: [Secret] -> [Secret] -> [Hash256] -> [Hash256] -> IO Bool
verifySecrets bSecrets aSecrets bHashes sumHashes = return True

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    g <- newGenIO :: IO CtrDRBG
    let (ys, (v:zs)) = splitAt (head $ crandomRs (0, length xs - 1) g) xs
    liftM ((:) v) $ shuffle (ys ++ zs)

hexTextToBs :: Text -> BL.ByteString
hexTextToBs = fromMaybe undefined . decodeHex . encodeUtf8
  where
    decodeHex bs =
        let (x, b) = B16.decode bs
        in guard (b == BL.empty) >> return x

aliceCommit = undefined
aliceClaim = undefined
