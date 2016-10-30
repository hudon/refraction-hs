{-# LANGUAGE OverloadedStrings #-}
{-|
  Discovery is the first step of Xim and allows peers to find eachother. This step
  adds Sybil attack resistance to the mixing
-}
module Network.Refraction.Discover
    ( discover
    , Location
    ) where

import Control.Concurrent.Chan (Chan)
import Network.Haskoin.Crypto (PrvKey)
import Network.Haskoin.Transaction (Tx)
import System.Random (getStdRandom, randomR)

import Network.Refraction.Discover.Advertiser
import Network.Refraction.Discover.Respondent
import Network.Refraction.Discover.Types
import Network.Refraction.PeerToPeer (Msg)


-- |Finds a mixing peer and returns its location to begin communication for fair exchange
discover :: Chan Msg -> Location -> Bool -> Bool -> PrvKey -> IO (Location, Tx)
discover chan myLoc isBob isAlice prv = flipCoin >>= pickRole
  where
    pickRole heads
      | isBob = runAdvertiser chan prv myLoc -- TODO: for debug purposes, to be removed
      | isAlice = runRespondent prv myLoc -- TODO: for debug purposes, to be removed
      | heads = runAdvertiser chan prv myLoc
      | otherwise = runRespondent prv myLoc
    -- TODO use crypto-api everywhere
    flipCoin = getStdRandom (randomR ((1 :: Int), 100)) >>= return . (> 50)
