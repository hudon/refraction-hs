module Network.Refraction.FairExchange.Alice
    ( runAlice
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (guard, liftM)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Word
import Debug.Trace
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util

import Network.Refraction.BitcoinUtils
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
    (aCommit, bCommit, bCommitRedeem) <- aliceCommit chan keypair bPub sumHashes lastTx theirLocation
    aliceClaim keypair aCommit bCommit bCommitRedeem mySecrets
    putStrLn "alice is done"

setupAliceSecrets :: Chan Msg
                  -> KeyPair
                  -> KeyPair
                  -> [Secret]
                  -> Location
                  -> IO (PubKey, PubKey, [Text])
setupAliceSecrets chan (_, aPub) (_, refundPub) mySecrets theirLocation = do
    putStrLn "Alice setting up secrets..."
    send theirLocation . BL.toStrict . A.encode $ AliceKeysMessage aPub refundPub mySecrets
    bobKeys <- liftM (fromMaybe undefined . A.decodeStrict') $ readChan chan
    indices <- liftM (take numChallenges) $ shuffle [0..numSecrets - 1]
    send theirLocation . BL.toStrict . A.encode $ indices
    bobSecrets <- liftM decodeSecrets (readChan chan)
    verifySecrets bobSecrets mySecrets (map decodeHashes $ bHashes bobKeys) (map decodeHashes $ bSumHashes bobKeys)
    putStrLn "Alice done setting up secrets!"
    return (bKey1 bobKeys, bKey2 bobKeys, bSumHashes bobKeys)
  where
    decodeHashes = either printErr id . S.decode . hexTextToBs
    decodeSecrets = fromMaybe undefined . A.decodeStrict'

verifySecrets :: [Secret] -> [Secret] -> [Hash256] -> [Hash256] -> IO Bool
verifySecrets bSecrets aSecrets bHashes sumHashes = return True

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    g <- newGenIO :: IO CtrDRBG
    let (ys, (v:zs)) = splitAt (head $ crandomRs (0, length xs - 1) g) xs
    liftM ((:) v) $ shuffle (ys ++ zs)

hexTextToBs :: Text -> B.ByteString
hexTextToBs = fromMaybe undefined . decodeHex . encodeUtf8
  where
    -- TODO(hudon): is this a re-implementation of Haskoin.Util.decodeHex?
    decodeHex bs =
        let (x, b) = B16.decode bs
        in guard (b == B.empty) >> return x

aliceCommit :: Chan Msg -> KeyPair -> PubKey -> [Text] -> Tx -> Location -> IO (Tx, Tx, Script)
aliceCommit chan (prv, pub) bPub sumHashes lastTx theirLocation = do
    putStrLn "Alice is committing..."
    bCommitRedeem <- liftM (either printErr id . S.decode . fromMaybe undefined . decodeHex) $ readChan chan
    print "received bob's redeem script"
    print bCommitRedeem
    --verifyRedeem bCommitRedeem
    send theirLocation . encodeHex $ S.encode "ok"
    bCommitHash <- liftM (either printErr id . S.decode . fromMaybe undefined . decodeHex) $ readChan chan
    print "received bob's commit hash"
    print bCommitHash
    bCommit <- liftM (fromMaybe undefined) $ fetchTx bCommitHash
    -- TODO(hudon) wait for bob commit confirmations
    --
    let (_:utxo:_) = getUTXOs lastTx
        -- TODO(hudon) clean up these fromEither/fromMaybe
        bsToHash256 bs = either (const Nothing) Just (S.decode bs)
        sumHashes256 = map (fromMaybe undefined . bsToHash256 . fromMaybe undefined . decodeHex . encodeUtf8) sumHashes
    let Right (tx, aCommitRedeem) = makeAliceCommit [utxo] [prv] pub bPub sumHashes256
    print "Sending redeem script"
    print aCommitRedeem
    send theirLocation . encodeHex $ S.encode aCommitRedeem
    bobVerification <- readChan chan
    broadcastTx tx
    putStrLn "Sending commit hash to bob..."
    print $ txHash tx
    send theirLocation . encodeHex . S.encode $ txHash tx
    putStrLn "Alice committed!"
    return (tx, bCommit, bCommitRedeem)

aliceClaim :: KeyPair -> Tx -> Tx -> Script -> [Integer] -> IO ()
aliceClaim (prv, pub) aCommit bCommit bCommitRedeem aSecrets = do
    putStrLn "Alice is claiming..."
    bClaim <- liftM (fromMaybe undefined) $ waitForBobClaim aCommit
    -- TODO(hudon) expand pattern we're matching on to catch errors
    let Right bSecret = getBSecret bClaim aSecrets
        (utxo:_) = getUTXOs bCommit
        Right tx = makeAliceClaim [utxo] [prv] bCommitRedeem bSecret pub
    broadcastTx tx
    putStrLn "Alice claimed!"

waitForBobClaim :: Tx -> IO (Maybe Tx)
waitForBobClaim tx = do
    putStrLn "Waiting for bob's claim..."
    fetchSpentTxId (txHash tx) 0 >>= maybe loop fetchTx
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
            -- TODO(hudon) in the future we'll only have (numSecrets - numChallenges) inputs
            opsSplit = splitAt numSecrets ops
            getPushData (OP_PUSHDATA bs _) = S.decode bs
        mapM getPushData $ fst opsSplit

-- TODO(hudon) implement me
verifyRedeem :: a -> Bool
verifyRedeem = const True

send :: Location -> Msg -> IO ()
send loc x = secureConnect loc $ sendMessage x

printErr x = traceShow x undefined
