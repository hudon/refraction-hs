{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Blockchain
    ( broadcast
    , transaction
    , utxos
    ) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, toJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.Serialize (decode)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Generator (SatoshiValue, UTXO(..))
import GHC.Generics (Generic)
import Network.Haskoin.Crypto (Address, addrToBase58)
import Network.Haskoin.Transaction (hexToTxHash, OutPoint(..), Tx, TxHash, TxOut(..), txHashToHex)
import Network.Haskoin.Util (decodeHex, decodeToMaybe)
import Network.HTTP.Client (HttpException(..))
import Network.Wreq

baseURL = "https://testnet.blockexplorer.com/api"

data TxPayload = TxPayload {
      rawtx :: Tx
} deriving (Generic)

instance FromJSON TxPayload

instance ToJSON TxPayload

data ResponseBroadcast = ResponseBroadcast {
      _transaction :: Tx
} deriving (Generic)

instance FromJSON ResponseBroadcast

broadcast :: Tx -> IO ()
broadcast tx = do
    let url = baseURL ++ "/tx/send"
    eresponse <- try $ post url (toJSON $ TxPayload tx)
    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> asJSON response >>= printTransaction
  where
    printTransaction r = putStrLn . show . _transaction $ r ^. responseBody

transaction :: TxHash -> IO Tx
transaction txhash = do
    let url = baseURL ++ "/rawtx/" ++ B8.unpack (txHashToHex txhash)
    r <- asJSON =<< get url
    return . rawtx $ r ^. responseBody

data ResponseUTXO = ResponseUTXO {
      txid :: TxHash
    , vout :: Word32
    , satoshis :: SatoshiValue
    , scriptPubKey :: Text
} deriving (Generic)

instance FromJSON ResponseUTXO

toUTXO :: ResponseUTXO -> UTXO
toUTXO r = UTXO txOut outPoint
  where
    txOut = TxOut (satoshis r) (fromMaybe undefined . decodeHex . encodeUtf8 . scriptPubKey $ r)
    outPoint = OutPoint (txid r)  (vout r)

utxos :: Address -> IO [UTXO]
utxos addr = do
    let url = baseURL ++ "/addr/" ++ B8.unpack (addrToBase58 addr) ++ "/utxo"
    r <- asJSON =<< get url
    return . map toUTXO $ r ^. responseBody
