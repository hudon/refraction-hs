{-# LANGUAGE OverloadedStrings #-}
module Discover
    ( postAd
    ) where

import Data.ByteString (ByteString)
import Network.Haskoin.Crypto (Address, PrvKey)
import Network.Haskoin.Transaction (Coin, coinValue, OutPoint, outValue, Tx, TxOut)


-- TODO(hudon): make this fee dynamic (set by user in config?)
-- Advertise fee
tao = 10000

type Location = ByteString

postAd :: IO Tx
postAd = undefined

discover :: IO Location
discover = undefined
-- if rand(0.5) > 0.5 then
--   assume role of Advertiser with address A and location alphaA
-- else
--   assume role of Respondent R with address R and location alphaR
-- Advertiser: publish T{A -> A, tip = tao/2, TEXT(loc=alphaA, nonce=nA, pool=P}
-- Respondent: Randomly select advertiser, store encAPK(nA, nR, alphaR) to alphaA
-- Advertiser: select respondent R, store sigAPK(nA paired to h(nR))" to alphaA
-- Respondent: publishes T{R -> R, tip = tao + extra, TEXT(id = encAPK(nA, nR))}
-- Advertiser: publishes T{A -> A, tip = tao/2, TEXT(lock = h(nR), nA)}
-- return aA to R, aR to A
