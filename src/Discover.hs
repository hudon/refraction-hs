{-# LANGUAGE OverloadedStrings #-}
{-|
  Discovery is the first step of Xim and allows peers to find eachother. This step
  adds Sybil attack resistance to the mixing
-}
module Discover
    ( discover
    , Location
    ) where

import Blockchain (broadcast)
import Control.Concurrent.Chan (Chan, readChan)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Generator (makeAdData, makeAdTransaction, SatoshiValue)
import Network.Haskoin.Transaction (Tx)
import PeerToPeer (Msg)
import System.Random (getStdRandom, randomR)

-- TODO(hudon): make this fee dynamic (set by user in config?)
-- Advertise fee
tao = 10000 :: SatoshiValue

type Location = ByteString

-- |Finds a mixing peer and returns its location to begin communication for fair exchange
discover :: Chan Msg -> Location -> Bool -> IO Location
discover chan myLoc isBob = flipCoin >>= pickRole
  where
    pickRole heads
      -- | heads = runAdvertiser chan
      | isBob = runAdvertiser chan
      | otherwise = runRespondent
    flipCoin = getStdRandom (randomR (1 :: Int, 100)) >>= return . (> 50)

selectAdvertiser :: IO Location
selectAdvertiser = return "E5RK5YGUTKXMIGGV.onion"

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
      putStrLn msg
      if n > 2 then pickRespondent else waitForRespondents (n + 1)
    pickRespondent = return 1

-- Advertiser: publishes T{A -> A, tip = tao/2, TEXT(lock = h(nR), nA)}
publishPairResponse :: IO ()
publishPairResponse = putStrLn "called publish pair response"

runAdvertiser :: Chan Msg -> IO Location
runAdvertiser chan = do
    publishAd
    r <- selectRespondent chan
    publishPairResponse
    return $ B8.pack "respondent-location.onion"

