{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    , makeAdData
    , makeAdTransaction
    , SatoshiValue
    , UTXO(..)
    ) where

import Data.ByteString (ByteString)
import Data.Word (Word64)
import Network.Haskoin.Crypto (Address, PrvKey)
import Network.Haskoin.Transaction (Coin, coinValue, OutPoint, outValue, Tx, TxOut)

type SatoshiValue = Word64

data UTXO = UTXO {
      _txOut :: TxOut
    , _outPoint :: OutPoint
} deriving (Show)

instance Coin UTXO where
    coinValue =  outValue . _txOut

makeSimpleTransaction :: [UTXO] -> [PrvKey] -> Address -> Either String Tx
makeSimpleTransaction utxos addr = undefined

-- |Takes coins to sign, the data to place in the OP_RETURN and the miner's fee value
makeAdTransaction :: [UTXO] -> [PrvKey] -> ByteString -> SatoshiValue -> Either String Tx
makeAdTransaction = undefined

-- |Takes a list of items and serializes so it is ready to be placed in a Tx
makeAdData :: [ByteString] -> ByteString
makeAdData = undefined
