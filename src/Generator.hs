{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    , makeOPRETURNTransaction
    , SatoshiValue
    ) where

import qualified Data.ByteString as B
import Data.Word (Word64)
import Network.Haskoin.Crypto (Address, PrvKey)
import Network.Haskoin.Transaction (Coin, coinValue, OutPoint, outValue, Tx, TxOut)

type SatoshiValue = Word64

data UTXO = UTXO {
      _txOut :: TxOut
    , _outPoint :: OutPoint
    , _prvKey :: PrvKey
}

instance Coin UTXO where
    coinValue =  outValue . _txOut

makeSimpleTransaction :: [UTXO] -> Address -> Either String Tx
makeSimpleTransaction utxos addr = undefined

makeOPRETURNTransaction :: [UTXO] -> B.ByteString -> Either String Tx
makeOPRETURNTransaction = undefined
