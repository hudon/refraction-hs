{-# LANGUAGE OverloadedStrings #-}
{-|
  Discovery is the first step of Xim and allows peers to find eachother. This step
  adds Sybil attack resistance to the mixing
-}
module Network.Refraction.Discover
    ( discover
    , Location
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (find)
import Data.Maybe
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import Network.Haskoin.Block (BlockHash)
import Network.Haskoin.Crypto (derivePubKey, PrvKey, pubKeyAddr)
import Network.Haskoin.Script (Script, ScriptOp(..), scriptOps)
import Network.Haskoin.Transaction (scriptOutput, Tx, txOut)
import Network.Refraction.Blockchain (broadcast, findOPRETURNs, utxos)
import Network.Refraction.Generator (makeAdTransaction, makePairRequest, SatoshiValue)
import Network.Refraction.PeerToPeer (Msg, sendMessage, unsecureSend)
import Network.Refraction.Tor(secureConnect)
import System.Random (getStdRandom, randomR)

-- TODO(hudon): make this fee dynamic (set by user in config?)
-- Advertise fee
tao = 10000 :: SatoshiValue

adFinder = "RFRCTN"
onionLengthWithoutTLD = 16

type Location = ByteString
type Nonce = Word64

-- |Finds a mixing peer and returns its location to begin communication for fair exchange
discover :: Chan Msg -> Location -> Bool -> Bool -> PrvKey -> IO Location
discover chan myLoc isBob isAlice prv = flipCoin >>= pickRole
  where
    pickRole heads
      | isBob = runAdvertiser chan prv myLoc -- TODO: for debug purposes, to be removed
      | isAlice = runRespondent prv -- TODO: for debug purposes, to be removed
      | heads = runAdvertiser chan prv myLoc
      | otherwise = runRespondent prv
    -- TODO use crypto-api everywhere
    flipCoin = getStdRandom (randomR ((1 :: Int), 100)) >>= return . (> 50)

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
publishPairRequest :: PrvKey -> (Nonce, Nonce) -> IO ()
publishPairRequest prvkey nonces = do
    putStrLn "publishing pair request..."
    let addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- utxos addr
    let tx = either undefined id $ makePairRequest utxos [prvkey] nonces tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "pair request published!"

-- Respondent: Randomly select advertiser, store encAPK(nA, nR, alphaR) to alphaA
runRespondent :: PrvKey -> IO Location
runRespondent prvkey = do
    (adLoc, aNonce) <- selectAdvertiser

    g <- newGenIO :: IO CtrDRBG
    let rNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
    -- TODO: we can't use Tor until we have the ad transaction on the blockchain
    -- stuff working because the locations are dynamic. Use direct connection for now.
    -- TODO: figure out if we should use lazy or strict... or at least fix this inefficiency
    secureConnect adLoc (sendMessage $ S.encodeLazy rNonce)
    --unsecureSend False "i am alice, wanna trade bitcoins?"
    --
    publishPairRequest prvkey (aNonce, rNonce)
    return $ B8.pack "advertiser-location.onion"

-- Advertiser: publish T{A -> A, tip = tao/2, TEXT(loc=alphaA, nonce=nA, pool=P}
publishAd :: PrvKey -> Location -> Nonce -> IO ()
publishAd prvkey loc nonce = do
    let addr = pubKeyAddr $ derivePubKey prvkey
        uniqueLoc = B8.take onionLengthWithoutTLD loc
    utxos <- utxos addr
    let tx = either undefined id $ makeAdTransaction utxos [prvkey] uniqueLoc nonce tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "ad published!"

-- Advertiser: select respondent R, store sigAPK(nA paired to h(nR))" to alphaA
selectRespondent :: Chan Msg -> IO Nonce
selectRespondent chan = putStrLn "called select respondent" >> waitForRespondents 0
  where
    waitForRespondents n = do
      msg <- readChan chan
      -- TODO have an actual picking mechanism
      if n == 0 then pickRespondent msg else waitForRespondents (n + 1)
    pickRespondent msg = do
        putStrLn "picked a respondent"
        return . either undefined id $ S.decodeLazy msg

-- Advertiser: publishes T{A -> A, tip = tao/2, TEXT(lock = h(nR), nA)}
publishPairResponse :: PrvKey -> (Nonce, Nonce) -> IO ()
publishPairResponse prvkey nonces = do
    putStrLn "publishing pair response"
    let addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- utxos addr
    -- TODO use a separate generator that hashes the respondent nonce
    let tx = either undefined id $ makePairRequest utxos [prvkey] nonces tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "pair response published!"

runAdvertiser :: Chan Msg -> PrvKey -> Location -> IO Location
runAdvertiser chan prvkey loc = do
    g <- newGenIO :: IO CtrDRBG
    let aNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
    publishAd prvkey loc aNonce
    rNonce <- selectRespondent chan
    publishPairResponse prvkey (aNonce, rNonce)
    return $ B8.pack "respondent-location.onion"

