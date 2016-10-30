{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Discover.Respondent
    ( runRespondent
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Haskoin.Crypto (derivePubKey, PrvKey, pubKeyAddr)
import Network.Haskoin.Script (Script, ScriptOp(..), scriptOps)
import Network.Haskoin.Transaction (Tx)
import Network.Refraction.Blockchain (broadcastTx, findOPRETURNs, fetchUTXOs)
import Network.Refraction.Discover.Types
import Network.Refraction.Generator (makePairRequest, SatoshiValue)
import Network.Refraction.PeerToPeer (Msg, sendMessage, unsecureSend)
import Network.Refraction.Tor(secureConnect)

runRespondent :: PrvKey -> Location -> IO (Location, Tx)
runRespondent prvkey myLoc = do
    putStrLn "Running respondent..."
    (adLoc, aNonce) <- selectAdvertiser

    g <- newGenIO :: IO CtrDRBG
    let rNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
    -- TODO: we can't use Tor until we have the ad transaction on the blockchain
    -- stuff working because the locations are dynamic. Use direct connection for now.
    -- TODO: figure out if we should use lazy or strict... or at least fix this inefficiency
    let msg = B.concat [S.encode rNonce, S.encode myLoc]
    secureConnect adLoc (sendMessage msg)
    --unsecureSend False "i am alice, wanna trade bitcoins?"
    --
    lastTx <- publishPairRequest prvkey (aNonce, rNonce)
    return (adLoc, lastTx)

selectAdvertiser :: IO (Location, Nonce)
selectAdvertiser = do
    putStrLn "Selecting advertiser..."
    ad@(loc, n) <- findAd
    let theirLocation = B8.concat [loc, encodeUtf8 ".onion"]
        nonce = either undefined id $ S.decode n
    putStrLn "Found advertiser"
    return (theirLocation, nonce)
  where
    findAd = findAd' []
    findAd' excludeHashes = do
        (opreturns, newExcludes) <- findOPRETURNs excludeHashes
        let adM = foldl parseAndChoose Nothing $ map scriptOps opreturns
        case adM of
            -- If there were no ads, sleep for 30 seconds and try again
            Nothing -> threadDelay 30000000 >> findAd' newExcludes
            Just ad -> return ad
    parseAndChoose prevAd newAd = if isJust prevAd then prevAd else parseAd newAd
    parseAd [OP_RETURN, OP_PUSHDATA bs _] = do
        remainder <- B8.stripPrefix (encodeUtf8 adFinder) bs
        return $ B8.splitAt onionLengthWithoutTLD remainder
    parseAd _ = Nothing

-- Respondent: publishes T{R -> R, tip = tao + extra, TEXT(id = encAPK(nA, nR))}
publishPairRequest :: PrvKey -> (Nonce, Nonce) -> IO Tx
publishPairRequest prvkey nonces = do
    putStrLn "Publishing pair request..."
    let addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- fetchUTXOs addr
    putStrLn "gathering utxos..."
    putStrLn $ show $ length utxos
    let tx = either undefined id $ makePairRequest utxos prvkey nonces tao (encodeUtf8 adFinder)
    broadcastTx tx
    putStrLn "Pair request published!"
    return tx
