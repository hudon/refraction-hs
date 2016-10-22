{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Discover.Advertiser
    ( runAdvertiser
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Haskoin.Crypto (derivePubKey, PrvKey, pubKeyAddr)

import Network.Refraction.Blockchain (broadcastTx, fetchUTXOs)
import Network.Refraction.PeerToPeer (Msg)
import Network.Refraction.Discover.Types
import Network.Refraction.Generator (makeAdTransaction, makePairRequest, SatoshiValue)

runAdvertiser :: Chan Msg -> PrvKey -> Location -> IO Location
runAdvertiser chan prvkey loc = do
    putStrLn "Running advertiser..."
    g <- newGenIO :: IO CtrDRBG
    let aNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
    publishAd prvkey loc aNonce
    (rNonce, rLoc) <- selectRespondent chan
    publishPairResponse prvkey (aNonce, rNonce)
    return rLoc

publishAd :: PrvKey -> Location -> Nonce -> IO ()
publishAd prvkey loc nonce = do
    putStrLn "Publish ad..."
    let addr = pubKeyAddr $ derivePubKey prvkey
        uniqueLoc = B8.take onionLengthWithoutTLD loc
    utxos <- fetchUTXOs addr
    let tx = either undefined id $ makeAdTransaction utxos prvkey uniqueLoc nonce tao (encodeUtf8 adFinder)
    broadcastTx tx
    putStrLn "Ad published!"

selectRespondent :: Chan Msg -> IO (Nonce, Location)
selectRespondent chan = do
    putStrLn "Selecting respondent..."
    respondent <- waitForRespondents 0
    putStrLn "Selected respondent!"
    return respondent
  where
    waitForRespondents n = do
      msg <- readChan chan
      -- TODO have an actual picking mechanism. We pick the first respondent for now
      if n == 0 then pickRespondent msg else waitForRespondents (n + 1)
    pickRespondent msg = do
        putStrLn "picked a respondent"
        let (rNonce, rLoc) = BL.splitAt 8 msg -- TODO: don't assume Word64 Nonce, use better schema
        return (either undefined id $ S.decodeLazy rNonce, either undefined id $ S.decodeLazy rLoc)

publishPairResponse :: PrvKey -> (Nonce, Nonce) -> IO ()
publishPairResponse prvkey nonces = do
    putStrLn "Publishing pair response"
    let addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- fetchUTXOs addr
    -- TODO use a separate generator that hashes the respondent nonce
    let tx = either undefined id $ makePairRequest utxos [prvkey] nonces tao (encodeUtf8 adFinder)
    broadcastTx tx
    putStrLn "Pair response published!"


