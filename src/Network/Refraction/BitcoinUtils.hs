{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.BitcoinUtils
  ( getUTXOs
  , makeKeyPair
  , makeTextKeyPair
  , SatoshiValue
  , UTXO(..)
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32, Word64)
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction

type SatoshiValue = Word64

data UTXO = UTXO {
      _txOut :: TxOut
    , _outPoint :: OutPoint
} deriving (Show)

instance Coin UTXO where
    coinValue =  outValue . _txOut


makeKeyPair :: IO (PrvKey, PubKey)
makeKeyPair = genKey >>= \p -> return (p, derivePubKey p)
  where
    genKey = withSource getEntropy genPrvKey

makeTextKeyPair :: IO (Text, Text)
makeTextKeyPair = do
  (prv, pub) <- makeKeyPair
  return ( decodeUtf8 $ encodePrvKey prv
         , decodeUtf8 . addrToBase58 $ pubKeyAddr pub)

-- TODO(hudon): This is getting outputs, they're not necessarily unspent... rename the data def?
getUTXOs :: Tx -> [UTXO]
getUTXOs tx =
  let txOuts = txOut tx
      os = zip [1..(length txOuts)] txOuts
      hash = txHash tx
      toUTXO (i, o) = UTXO o $ OutPoint hash (fromIntegral $ i - 1)
  in map toUTXO os
