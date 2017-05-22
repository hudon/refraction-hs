{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.RoundManager
    ( prepareRounds
    ) where

import Network (PortNumber)
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT

import Network.Refraction.BitcoinUtils
import Network.Refraction.Blockchain
import Network.Refraction.Discover (discover)
import Network.Refraction.FairExchange (fairExchange)
import Network.Refraction.Generator (makeSplitTransaction)
import qualified Network.Refraction.PeerToPeer as P2P
import Network.Refraction.Tor (makeHiddenService)

-- TODO(hudon): don't use this...
fromEither = either (\x -> print x >> undefined) return

-- TODO(hudon): actually use `addr` and `refundAddr`
startRound :: Bool -> Bool -> HC.Address -> HC.Address -> HC.PrvKey -> UTXO -> IO ()
startRound isBob isAlice addr refundAddr prv utxo = do
    -- TODO(hudon): better port allocation (right now the other rounds will fail)
    -- TODO(hudon): make the rounds not fetch all UTXOS but only the split output assigned
    --              to the round
    putStrLn "Round started."
    let port = if isBob then 4142 else 4143 :: PortNumber
    chan <- P2P.startServer port
    putStrLn "Making hidden service..."
    makeHiddenService port $ \myLocation -> do
        putStrLn $ "hidden service location: " ++ show myLocation
        (theirLocation, lastTx) <- discover chan myLocation isBob isAlice prv
        putStrLn "discover done!"
        fairExchange isBob isAlice prv chan lastTx myLocation theirLocation

--SEE XIM PAPER:
--m: number of parallel rounds, determined by how much money Alice has
--n: number of sequential rounds, determined by the pool
--delta: the mix unit. large enough to not be too expensive on fees. small enough to get participants
-- 1. divide input amount into m mix units of size delta
-- 2. each mix unit delta will go through n rounds

prepareRounds :: Bool -> Bool -> HC.PrvKey -> HC.Address -> HC.Address -> IO ()
prepareRounds isBob isAlice startPrv endAddr refundAddr = do
  -- TODO: add timeout to waits across app
  -- TODO for round timeouts (if no peer is found for that unit), the funds should go back to refundAddr
  -- TODO(hudon): once we use the startPrvKey (or startAddr), wait for it to receive funds before continuing
  utxos <- fetchUTXOs $ toAddr startPrv
  splitTx <- fromEither $ makeSplitTransaction utxos [startPrv] refundAddr
  broadcastTx splitTx
  _ <- fetchTxWaitForRelay $ HT.txHash splitTx
  putStrLn "Starting rounds..."
  -- TODO(hudon): run in parallel
  mapM_ start $ getUTXOs splitTx
  where
    toAddr = HC.pubKeyAddr . HC.derivePubKey
    start = startRound isBob isAlice endAddr refundAddr startPrv
