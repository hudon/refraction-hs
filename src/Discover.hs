{-# LANGUAGE OverloadedStrings #-}
{-|
  Discovery is the first step of Xim and allows peers to find eachother. This step
  adds Sybil attack resistance to the mixing
-}
module Discover
    ( discover
    , Location
    ) where

import Blockchain (broadcast, findOPRETURNs)
import Control.Concurrent.Chan (Chan, readChan)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.List (find)
import Control.Concurrent (threadDelay)
import Data.Serialize as S
import Data.Text.Encoding (decodeUtf8)
import Generator (makeAdData, makeAdTransaction, SatoshiValue)
import Network.Haskoin.Block (BlockHash)
import Network.Haskoin.Script (Script, ScriptOp(..), scriptOps)
import Network.Haskoin.Transaction (scriptOutput, Tx, txOut)
import PeerToPeer (Msg, sendMessage, unsecureSend)
import System.Random (getStdRandom, randomR)
import Tor(secureConnect)

-- TODO(hudon): make this fee dynamic (set by user in config?)
-- Advertise fee
tao = 10000 :: SatoshiValue

type Location = ByteString

-- |Finds a mixing peer and returns its location to begin communication for fair exchange
discover :: Chan Msg -> Location -> Bool -> Bool -> IO Location
discover chan myLoc isBob isAlice = flipCoin >>= pickRole
  where
    pickRole heads
      | isBob = runAdvertiser chan -- TODO: for debug purposes, to be removed
      | isAlice = runRespondent -- TODO: for debug purposes, to be removed
      | heads = runAdvertiser chan
      | otherwise = runRespondent
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
        let adM = find isAd opreturns
        case adM of
            -- If there were no ads, sleep for 10 seconds and try again
            Nothing -> threadDelay 10000000 >> findAd' newExcludes
            Just ad -> return ad
    isAd [OP_RETURN, OP_PUSHDATA bs _] = "RFRCTN" == decodeUtf8 bs
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
publishAd :: IO ()
publishAd = do
    let nonce = undefined
    let location = undefined
    let opreturnData = makeAdData [location, nonce]
    let utxos = undefined
    let keys = undefined
    let tx = either undefined id $ makeAdTransaction utxos keys opreturnData tao :: Tx
    --broadcast tx
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

runAdvertiser :: Chan Msg -> IO Location
runAdvertiser chan = do
    publishAd
    r <- selectRespondent chan
    publishPairResponse
    return $ B8.pack "respondent-location.onion"

