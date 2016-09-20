{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    , makeAdData
    , makeAdTransaction
    , SatoshiValue
    , UTXO(..)
    ) where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import Network.Haskoin.Crypto (PrvKey, Address)
import qualified Network.Haskoin.Script as S
import qualified Network.Haskoin.Transaction as T

type SatoshiValue = Word64

data UTXO = UTXO {
      _txOut :: T.TxOut
    , _outPoint :: T.OutPoint
} deriving (Show)

instance T.Coin UTXO where
    coinValue =  T.outValue . _txOut

-- Default transaction fee
dTxFee = 10000

-- |Takes a UTXO and returns a SigInput that can be used to sign a Tx
mkInput :: UTXO -> T.SigInput
mkInput utxo = T.SigInput pso (_outPoint utxo) (S.SigAll False) Nothing
  where pso = either undefined id . S.decodeOutputBS . T.scriptOutput $ _txOut utxo

-- |Takes a list of utxos and associated private keys and pays to an address
makeSimpleTransaction :: [UTXO] -> [PrvKey] -> Address -> Either String T.Tx
makeSimpleTransaction utxos prvkeys addr =
    T.signTx tx (map mkInput utxos) prvkeys
  where
    sumVal v utxo = v + T.coinValue utxo - dTxFee
    tx = either undefined id $ T.buildTx (map _outPoint utxos) [(S.PayPKHash addr, foldl sumVal 0 utxos)]

-- |Takes coins to sign, the data to place in the OP_RETURN and the miner's fee value
makeAdTransaction :: [UTXO] -> [PrvKey] -> ByteString -> SatoshiValue -> Either String T.Tx
makeAdTransaction = undefined

-- |Takes a list of items and serializes so it is ready to be placed in a Tx
makeAdData :: [ByteString] -> ByteString
makeAdData = undefined
