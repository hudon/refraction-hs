{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Discover.Advertiser
    ( runAdvertiser
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (find)
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Haskoin.Crypto (derivePubKey, PrvKey, pubKeyAddr)
import Network.Haskoin.Script (decodeOutputBS, isDataCarrier)
import Network.Haskoin.Transaction (scriptOutput, Tx)

import Network.Refraction.BitcoinUtils
import Network.Refraction.Blockchain (broadcastTx, fetchUTXOs)
import Network.Refraction.Discover.Types
import Network.Refraction.Generator (makeAdTransaction, makePairRequest)
import Network.Refraction.PeerToPeer (Msg)

-- TODO(hudon): don't use this...
fromEither = either undefined id
-- TODO(hudon): avoid partial functions https://wiki.haskell.org/Avoiding_partial_functions

runAdvertiser :: Chan Msg -> PrvKey -> UTXO -> Location -> IO (Location, Tx)
runAdvertiser chan prvkey utxo loc = do
    putStrLn "Running advertiser..."
    g <- newGenIO :: IO CtrDRBG
    let aNonce = head $ crandomRs (minBound, maxBound) g :: Nonce
    adTx <- publishAd prvkey utxo loc aNonce
    (rNonce, rLoc) <- selectRespondent chan
    tx <- publishPairResponse prvkey (getChangeUTXO adTx) (aNonce, rNonce)
    return (rLoc, tx)
  where
    getChangeUTXO tx = case find isNotOPRETURN (getUTXOs tx) of
      -- TODO(hudon): handle error
      Nothing -> undefined
      Just utxo -> utxo
    isNotOPRETURN = not . isDataCarrier . fromEither . decodeOutputBS . scriptOutput . _txOut

publishAd :: PrvKey -> UTXO -> Location -> Nonce -> IO Tx
publishAd prvkey utxo loc nonce = do
    putStrLn "Publish ad..."
    let uniqueLoc = B8.take onionLengthWithoutTLD loc
        tx = either undefined id $ makeAdTransaction [utxo] prvkey uniqueLoc nonce tao (encodeUtf8 adFinder)
    broadcastTx tx
    putStrLn "Ad published!"
    return tx

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
        let (rNonce, rLoc) = B.splitAt 8 msg -- TODO: don't assume Word64 Nonce, use better schema
        return (either undefined id $ S.decode rNonce, either undefined id $ S.decode rLoc)

publishPairResponse :: PrvKey -> UTXO -> (Nonce, Nonce) -> IO Tx
publishPairResponse prvkey utxo nonces = do
    putStrLn "Publishing pair response"
    -- TODO use a separate generator that hashes the respondent nonce
    let tx = either undefined id $ makePairRequest [utxo] prvkey nonces tao (encodeUtf8 adFinder)
    broadcastTx tx
    putStrLn "Pair response published!"
    return tx
