module Network.Refraction.FairExchange.Bob
    ( runBob
    ) where

import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (liftM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util
import Network.Refraction.Blockchain (broadcastTx, fetchTx, fetchUTXOs)
import Network.Refraction.FairExchange.Types
-- TODO(hudon): doesn't make sense to depend on Discover for location type
import Network.Refraction.Discover (Location)
import Network.Refraction.Generator
import Network.Refraction.PeerToPeer (Msg, sendMessage)
import Network.Refraction.Tor(secureConnect)

-- TODO(hudon): for now we pay back to the initial keypair for the claim tx
-- but in the future claim should go to a completely new pubkey

runBob :: Chan Msg -> KeyPair -> KeyPair -> Tx -> [Secret] -> Location -> IO ()
runBob chan keypair refundKeypair lastTx mySecrets theirLocation = do
    putStrLn "Running Bob fair exchange protocol..."
    -- TODO(hudon) hash here rather than in setup
    (aPub, aRefundPub, bHashes, sums) <- setupBobSecrets chan keypair refundKeypair mySecrets theirLocation
    (aCommit, aCommitRedeem) <- bobCommit chan keypair aPub bHashes lastTx theirLocation
    bobClaim keypair aCommit aCommitRedeem sums
    putStrLn "bob is done"

setupBobSecrets :: Chan Msg
                -> KeyPair
                -> KeyPair
                -> [Secret]
                -> Location
                -> IO (PubKey, PubKey, [Text], [Integer])
setupBobSecrets chan (_, bPub) (_, refundPub) mySecrets theirLocation = do
    putStrLn "Bob setting up secrets..."
    aliceKeys <- liftM (fromMaybe undefined . A.decode) $ readChan chan
    let aliceSecrets = aSecrets aliceKeys
        bobHashes = map hashAndEncode mySecrets
        sums = zipWith (+) aliceSecrets mySecrets
        sumHashes = map hashAndEncode sums
    send theirLocation . A.encode $ BobKeyMessage bPub refundPub bobHashes sumHashes
    indices <- liftM (fromMaybe undefined . A.decode) $ readChan chan
    -- TODO(hudon) we need to remove these secrets from the ones that will be used for committing
    send theirLocation . A.encode $ map ((!!) mySecrets) indices
    putStrLn "Bob done setting up secrets!"
    return (aKey1 aliceKeys, aKey2 aliceKeys, bobHashes, sums)
  where
    hashAndEncode = bsToHexText . S.encodeLazy . doubleHash256 . S.encode

bsToHexText :: BL.ByteString -> Text
bsToHexText = decodeUtf8 . B16.encode

bobCommit :: Chan Msg -> KeyPair -> PubKey -> [Text] -> Tx -> Location -> IO (Tx, Script)
bobCommit chan (prv, pub) aPub bHashes lastTx theirLocation = do
    putStrLn "Bob is committing..."
    let utxo = makeUTXO lastTx 1
    -- TODO(hudon) don't do this partial pattern...
    let bHashes256 = map (fromMaybe undefined . bsToHash256 . fromMaybe undefined . decodeHex . BL.toStrict .  encodeUtf8) bHashes
    let Right (tx, bCommitRedeem) = makeBobCommit [utxo] [prv] aPub pub bHashes256
    send theirLocation $ S.encodeLazy bCommitRedeem
    aliceVerification <- readChan chan
    broadcastTx tx
    putStrLn "Sending commit hash to alice..."
    send theirLocation $ S.encodeLazy $ txHash tx
    -- alice's commit:
    aCommitRedeem <- liftM (either undefined id . S.decodeLazy) $ readChan chan
    --verifyRedeem aCommitRedeem
    send theirLocation $ S.encodeLazy "ok"
    aCommitHash <- liftM (either undefined id . S.decodeLazy) $ readChan chan
    -- TODO (hudon) the transaction might not be fetchable yet (has to be relayed)
    aCommit <- fetchTx aCommitHash
    putStrLn "Bob committed!"
    return (aCommit, aCommitRedeem)

bobClaim :: KeyPair -> Tx -> Script -> [Integer] -> IO ()
bobClaim (prv, pub) aCommit aCommitRedeem sums = do
    putStrLn "Bob is claiming..."
    let utxo = makeUTXO aCommit 0
    -- TODO don't make these arguments lists
    let Right tx = makeBobClaim [utxo] [prv] aCommitRedeem sums pub
    broadcastTx tx
    putStrLn "Bob claimed!"

verifyRedeem :: a -> Bool
verifyRedeem = const True

makeUTXO :: Tx -> Int -> UTXO
makeUTXO tx index =
    let op = OutPoint (txHash tx) (fromIntegral index)
        to = txOut tx !! index
    in UTXO to op

send :: Location -> Msg -> IO ()
send loc x = secureConnect loc $ sendMessage x
