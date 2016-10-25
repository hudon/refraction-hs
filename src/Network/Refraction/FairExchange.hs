{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.FairExchange
    ( fairExchange
    ) where

import Data.Aeson (decode, encode, FromJSON, ToJSON)
import Control.Concurrent.Chan (Chan)
import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import Network.Haskoin.Crypto
import Network.Refraction.Discover (Location)
import Network.Refraction.FairExchange.Alice
import Network.Refraction.FairExchange.Bob
import Network.Refraction.FairExchange.Types
import Network.Refraction.PeerToPeer (Msg)

fairExchange :: Bool
             -> Bool
             -> PrvKey
             -> Chan Msg
             -> Location
             -> Location
             -> IO ()
fairExchange isBob isAlice prv chan _ theirLocation = do
    putStrLn "fairExchange called"
    refundKeypair <- makeKeyPair
    mySecrets <- genSecrets numSecrets
    let run = if isBob then runBob else runAlice
        lastTx = undefined
    run chan (prv, derivePubKey prv) refundKeypair lastTx mySecrets theirLocation
  where
    genKey = withSource getEntropy genPrvKey
    makeKeyPair = genKey >>= \p -> return (p, derivePubKey p)

genSecrets :: Int -> IO [Integer]
genSecrets n = do
    -- TODO: just use 1 generator for FairExchange module? instead of using a new one to shuffle later
    g <- newGenIO :: IO CtrDRBG
    return . take n $ crandomRs (-2^255, 2^255-1) g
