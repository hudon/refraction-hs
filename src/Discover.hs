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
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
import Generator (makeAdTransaction, SatoshiValue)
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

type Location = ByteString

-- |Finds a mixing peer and returns its location to begin communication for fair exchange
discover :: Chan Msg -> Location -> Bool -> Bool -> PrvKey -> IO Location
discover chan myLoc isBob isAlice prv = flipCoin >>= pickRole
  where
    pickRole heads
      | isBob = runAdvertiser chan prv myLoc -- TODO: for debug purposes, to be removed
      | isAlice = runRespondent -- TODO: for debug purposes, to be removed
      | heads = runAdvertiser chan prv myLoc
      | otherwise = runRespondent
    -- TODO use crypto-api everywhere
    flipCoin = getStdRandom (randomR ((1 :: Int), 100)) >>= return . (> 50)

selectAdvertiser :: IO Location
selectAdvertiser = do
    let theirLocation = "L4TSUOJSZU23TPQZ.onion"
    ad <- findAd
    -- TODO: we can't use Tor until we have the ad transaction on the blockchain
    -- stuff working because the locations are dynamic. Use direct connection for now.
    --secureConnect theirLocation (sendMessage "i am alice, wanna trade bitcoins?")
    unsecureSend False "i am alice, wanna trade bitcoins?"
    return theirLocation
  where
    findAd = findAd' []
    findAd' excludeHashes = do
        (opreturns, newExcludes) <- findOPRETURNs excludeHashes
        let adM = find (isAd . scriptOps) opreturns
        case adM of
            -- If there were no ads, sleep for 30 seconds and try again
            Nothing -> threadDelay 30000000 >> findAd' newExcludes
            Just ad -> return ad
    isAd [OP_RETURN, OP_PUSHDATA bs _] = adFinder == decodeUtf8 bs
    isAd _ = False

-- Respondent: publishes T{R -> R, tip = tao + extra, TEXT(id = encAPK(nA, nR))}
publishPairRequest :: IO ()
publishPairRequest = return ()

-- Respondent: Randomly select advertiser, store encAPK(nA, nR, alphaR) to alphaA
runRespondent :: IO Location
runRespondent = do
    selectAdvertiser
    publishPairRequest
    return $ B8.pack "advertiser-location.onion"

-- Advertiser: publish T{A -> A, tip = tao/2, TEXT(loc=alphaA, nonce=nA, pool=P}
publishAd :: PrvKey -> Location -> IO ()
publishAd prvkey loc = do
    g <- newGenIO :: IO CtrDRBG
    let nonce = head $ crandomRs (minBound, maxBound) g :: Word64
        addr = pubKeyAddr $ derivePubKey prvkey
    utxos <- utxos addr
    let tx = either undefined id $ makeAdTransaction utxos [prvkey] loc nonce tao (encodeUtf8 adFinder)
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

