module Network.Refraction.FairExchange.Alice
    ( runAlice
    ) where

import Control.Concurrent (threadDelay)
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
import Data.Word
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util
import Network.Refraction.Blockchain (broadcastTx, fetchTx, fetchSpentTxId)
import Network.Refraction.Discover (Location)
import Network.Refraction.FairExchange.Types
import Network.Refraction.Generator
import Network.Refraction.PeerToPeer (Msg, sendMessage)
import Network.Refraction.Tor(secureConnect)

runAlice :: Chan Msg -> KeyPair -> KeyPair -> Tx -> [Secret] -> Location -> IO ()
runAlice chan keypair refundKeypair lastTx mySecrets theirLocation = do
    putStrLn "Running Alice fair exchange protocol..."
    (bPub, bRefundPub, sumHashes) <- setupAliceSecrets chan keypair refundKeypair mySecrets theirLocation
    (bCommit, bCommitRedeem) <- aliceCommit chan keypair bPub sumHashes lastTx theirLocation
    aliceClaim keypair bCommit bCommitRedeem mySecrets
    putStrLn "alice is done"

setupAliceSecrets :: Chan Msg
                  -> KeyPair
                  -> KeyPair
                  -> [Secret]
                  -> Location
                  -> IO (PubKey, PubKey, [Text])
setupAliceSecrets chan (_, aPub) (_, refundPub) mySecrets theirLocation = do
    send theirLocation . A.encode $ AliceKeysMessage aPub refundPub mySecrets
    bobKeys <- liftM (fromMaybe undefined . A.decode) $ readChan chan
    indices <- liftM (take numChallenges) $ shuffle [1..numSecrets]
    send theirLocation . A.encode $ indices
    bobSecrets <- liftM decodeSecrets (readChan chan)
    verifySecrets bobSecrets mySecrets (map decodeHashes $ bHashes bobKeys) (map decodeHashes $ bSumHashes bobKeys)
    return (bKey1 bobKeys, bKey2 bobKeys, bSumHashes bobKeys)
  where
    decodeHashes = either undefined id . S.decodeLazy . hexTextToBs
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

aliceCommit :: Chan Msg -> KeyPair -> PubKey -> [Text] -> Tx -> Location -> IO (Tx, Script)
aliceCommit chan (prv, pub) bPub sumHashes lastTx theirLocation = do
    bCommitRedeem <- readChan chan
    --verifyRedeem bCommitRedeem
    send theirLocation $ S.encodeLazy "ok"
    bCommitHash <- liftM (either undefined id . S.decodeLazy) $ readChan chan
    bCommit <- fetchTx bCommitHash
    -- TODO(hudon) wait for bob commit confirmations
    --
    let utxo = makeUTXO lastTx 1
        sumHashes256 = map (fromMaybe undefined . bsToHash256 . fromMaybe undefined . decodeHex . BL.toStrict .  encodeUtf8) sumHashes
    let Right (tx, aCommitRedeem) = makeAliceCommit [utxo] [prv] bPub pub sumHashes256
    send theirLocation $ S.encodeLazy aCommitRedeem
    bobVerification <- readChan chan
    aCommitHash <- broadcastTx tx
    send theirLocation $ S.encodeLazy aCommitHash
    return (bCommit, either undefined id $ S.decodeLazy bCommitRedeem)

aliceClaim :: KeyPair -> Tx -> Script -> [Integer] -> IO ()
aliceClaim (prv, pub) bCommit bCommitRedeem aSecrets = do
    bClaim <- waitForBobClaim bCommit
    -- TODO(hudon) expand pattern we're matching on to catch errors
    let Right bSecret = getBSecret bClaim aSecrets
        utxo = makeUTXO bCommit 0
        Right tx = makeAliceClaim [utxo] [prv] bCommitRedeem bSecret pub
    broadcastTx tx

waitForBobClaim :: Tx -> IO Tx
waitForBobClaim tx = do
    putStrLn "Waiting for bob's claim..."
    spentTxId <- fetchSpentTxId (txHash tx) 0
    maybe loop fetchTx spentTxId
  where loop = threadDelay 30000000 >> waitForBobClaim tx

-- TODO(hudon) don't assume it's the first input
getBSecret :: Tx -> [Integer] -> Either String Integer
getBSecret tx aSecrets = do
    sums <- decodeSums $ head $ txIn tx
    return $ (head sums) - (head aSecrets)
  where
    decodeSums txin = do
        r <- S.decode $ scriptInput txin
        let ops = scriptOps r
            opsSplit = splitAt (numSecrets - numChallenges) ops
            getPushData (OP_PUSHDATA bs _) = S.decode bs
        mapM getPushData $ fst opsSplit

-- TODO(hudon) implement me
verifyRedeem :: a -> Bool
verifyRedeem = const True

makeUTXO :: Tx -> Word32 -> UTXO
makeUTXO tx index =
    let op = OutPoint (txHash tx) index
        to = txOut tx !! (fromIntegral index)
    in UTXO to op

send :: Location -> Msg -> IO ()
send loc x = secureConnect loc $ sendMessage x
