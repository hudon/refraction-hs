{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Blockchain
    ( broadcast
    , utxos
    ) where

import Control.Lens
import Data.Aeson (FromJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.Serialize (decode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.Haskoin.Crypto (Address, addrToBase58)
import Network.Haskoin.Transaction (hexToTxHash, Tx, TxHash, txHashToHex)
import Network.Haskoin.Util (decodeHex, decodeToMaybe)
import Network.Wreq

baseURL = "https://testnet.blockexplorer.com/api"

broadcast :: Tx -> IO ()
broadcast tx = undefined

data ResponseTransaction = ResponseTransaction {
      rawtx :: Text
} deriving (Generic)

instance FromJSON ResponseTransaction

transaction :: TxHash -> IO Tx
transaction txhash = do
    let url = baseURL ++ "/rawtx/" ++ B8.unpack (txHashToHex txhash)
    r <- asJSON =<< get url
    let tx = decode . fromMaybe undefined . decodeHex . encodeUtf8 . rawtx $ r ^. responseBody
    return $ either undefined id tx -- YOLO

data ResponseUTXO = ResponseUTXO {
      txid :: Text
    , vout :: Int
} deriving (Generic)

instance FromJSON ResponseUTXO

utxos :: Address -> IO [(Tx, Int)]
utxos addr = do
    let url = baseURL ++ "/addr/" ++ B8.unpack (addrToBase58 addr) ++ "/utxo"
    r <- asJSON =<< get url
    let utxos = r ^. responseBody :: [ResponseUTXO]
    sequence $ map fetchUTXO utxos
  where
    fetchUTXO utxo = getTx utxo >>= (\tx -> return (tx, vout utxo))
    getTx = transaction . fromMaybe undefined . hexToTxHash . encodeUtf8 . txid
