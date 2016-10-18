{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Discover.Advertiser
    ( runAdvertiser
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.ByteString.Char8 as B8
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Haskoin.Crypto (derivePubKey, PrvKey, pubKeyAddr)

import Network.Refraction.Blockchain (broadcast, utxos)
import Network.Refraction.PeerToPeer (Msg)
import Network.Refraction.Discover.Types
import Network.Refraction.Generator (makeAdTransaction, makePairRequest, SatoshiValue)

runAdvertiser :: Chan Msg -> PrvKey -> Location -> IO Location
runAdvertiser chan prvkey loc = do
    g <- newGenIO :: IO CtrDRBG
    let aNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
    publishAd prvkey loc aNonce
    rNonce <- selectRespondent chan
    publishPairResponse prvkey (aNonce, rNonce)
    return $ B8.pack "respondent-location.onion"

publishAd :: PrvKey -> Location -> Nonce -> IO ()
publishAd prvkey loc nonce = do
    let addr = pubKeyAddr $ derivePubKey prvkey
        uniqueLoc = B8.take onionLengthWithoutTLD loc
    utxos <- utxos addr
    let tx = either undefined id $ makeAdTransaction utxos [prvkey] uniqueLoc nonce tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "ad published!"

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

publishPairResponse :: PrvKey -> (Nonce, Nonce) -> IO ()
publishPairResponse prvkey nonces = do
    putStrLn "publishing pair response"
    let addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- utxos addr
    -- TODO use a separate generator that hashes the respondent nonce
    let tx = either undefined id $ makePairRequest utxos [prvkey] nonces tao (encodeUtf8 adFinder)
    broadcast tx
    putStrLn "pair response published!"


