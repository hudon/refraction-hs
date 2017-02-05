{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.BitcoinUtils
  ( makeAddress
  , makeKeyPair
  ) where

import Data.ByteString
import Network.Haskoin.Crypto

makeKeyPair :: IO (PrvKey, PubKey)
makeKeyPair = genKey >>= \p -> return (p, derivePubKey p)
  where
    genKey = withSource getEntropy genPrvKey

makeAddress :: IO (PrvKey, ByteString)
makeAddress = do
  (prv, pub) <- makeKeyPair
  return (prv, addrToBase58 $ pubKeyAddr pub)
