{-# LANGUAGE OverloadedStrings #-}
{-|
  Discovery is the first step of Xim and allows peers to find eachother. This step
  adds Sybil attack resistance to the mixing
-}
module Discover
    ( discover
    , Location
    ) where

import Blockchain (broadcast, findOPRETURNs, utxos)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.List (find)
import Data.Maybe
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import Generator (makeAdTransaction, makePairRequest, SatoshiValue)
import Network.Haskoin.Block (BlockHash)
import Network.Haskoin.Crypto (derivePubKey, PrvKey, pubKeyAddr)
import Network.Haskoin.Script (Script, ScriptOp(..), scriptOps)
import Network.Haskoin.Transaction (scriptOutput, Tx, txOut)
import PeerToPeer (Msg, sendMessage, unsecureSend)
import System.Random (getStdRandom, randomR)
import Tor(secureConnect)

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
    ad@(loc, n) <- findAd
    let theirLocation = B8.concat [loc, encodeUtf8 ".onion"]
        nonce = either undefined id $ S.decode n
    -- TODO: we can't use Tor until we have the ad transaction on the blockchain
    -- stuff working because the locations are dynamic. Use direct connection for now.
    secureConnect theirLocation (sendMessage "i am alice, wanna trade bitcoins?")
    --unsecureSend False "i am alice, wanna trade bitcoins?"
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
publishPairRequest :: PrvKey -> Nonce -> IO Nonce
publishPairRequest prvkey aNonce = do
    g <- newGenIO :: IO CtrDRBG
    let rNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
        addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- utxos addr
    let tx = either undefined id $ makePairRequest utxos [prvkey] (aNonce, rNonce) tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "pair request published!"
    return rNonce

-- Respondent: Randomly select advertiser, store encAPK(nA, nR, alphaR) to alphaA
runRespondent :: PrvKey -> IO Location
runRespondent prvkey = do
    (loc, nonce) <- selectAdvertiser
    publishPairRequest prvkey nonce
    return $ B8.pack "advertiser-location.onion"

-- Advertiser: publish T{A -> A, tip = tao/2, TEXT(loc=alphaA, nonce=nA, pool=P}
publishAd :: PrvKey -> Location -> IO ()
publishAd prvkey loc = do
    g <- newGenIO :: IO CtrDRBG
    let nonce = head $ crandomRs (minBound, maxBound) g :: Nonce
        addr = pubKeyAddr $ derivePubKey prvkey
        uniqueLoc = B8.take onionLengthWithoutTLD loc
    utxos <- utxos addr
    let tx = either undefined id $ makeAdTransaction utxos [prvkey] uniqueLoc nonce tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "ad published!"

-- Advertiser: select respondent R, store sigAPK(nA paired to h(nR))" to alphaA
selectRespondent :: Chan Msg -> IO Int
selectRespondent chan = putStrLn "called select respondent" >> waitForRespondents 0
  where
    waitForRespondents n = do
      msg <- readChan chan
      -- TODO have an actual picking mechanism
      if n == 0 then pickRespondent else waitForRespondents (n + 1)
    pickRespondent = putStrLn "picked a respondent" >> return 1

-- Advertiser: publishes T{A -> A, tip = tao/2, TEXT(lock = h(nR), nA)}
publishPairResponse :: IO ()
publishPairResponse = putStrLn "called publish pair response"

runAdvertiser :: Chan Msg -> PrvKey -> Location -> IO Location
runAdvertiser chan prvkey loc = do
    publishAd prvkey loc
    r <- selectRespondent chan
    publishPairResponse
    return $ B8.pack "respondent-location.onion"

