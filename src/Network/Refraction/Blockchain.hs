{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Network.Refraction.Blockchain
    ( broadcast
    , findOPRETURNs
    , transaction
    , utxos
    ) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (_String, key)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (union)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.Haskoin.Block (Block, BlockHash, blockHashToHex, blockHeader, blockTxns, headerHash, hexToBlockHash, prevBlock)
import Network.Haskoin.Crypto (Address, addrToBase58)
import Network.Haskoin.Script (Script(..), ScriptOp(..))
import Network.Haskoin.Transaction (hexToTxHash, OutPoint(..), scriptOutput, Tx, TxHash, txOut, TxOut(..), txHashToHex)
import Network.Haskoin.Util (decodeHex, decodeToMaybe)
import Network.HTTP.Client (HttpException(..))
import Network.Refraction.Generator (SatoshiValue, UTXO(..))
import Network.Wreq

baseURL = "https://testnet.blockexplorer.com/api"

data TxPayload = TxPayload {
      rawtx :: Tx
} deriving (Generic)

instance FromJSON TxPayload

instance ToJSON TxPayload

data ResponseBroadcast = ResponseBroadcast {
      broadcastTxid :: TxHash
} deriving (Show)

instance FromJSON ResponseBroadcast where
    parseJSON (Object m) = ResponseBroadcast <$> m .: "txid"

broadcast :: Tx -> IO ()
broadcast tx = do
    putStrLn "Broadcasting"
    let url = baseURL ++ "/tx/send"
    eresponse <- try $ post url (toJSON $ TxPayload tx)
    case eresponse of
        Left e -> print (e :: HttpException)
        Right response -> asJSON response >>= printTransaction
  where
    printTransaction r = putStrLn . show . broadcastTxid $ r ^. responseBody

transaction :: TxHash -> IO Tx
transaction txhash = do
    let url = baseURL ++ "/rawtx/" ++ B8.unpack (txHashToHex txhash)
    r <- asJSON =<< get url
    return . rawtx $ r ^. responseBody

data ResponseUTXO = ResponseUTXO {
      txid :: TxHash
    , vout :: Word32
    , satoshis :: SatoshiValue
    , scriptPubKey :: T.Text
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

findOPRETURNs :: [BlockHash] -> IO ([Script], [BlockHash])
findOPRETURNs excludes = do
    blocks <- fetchRecentBlocks excludes
    let findTxns = filter isDataCarrier . map firstScript . concat . map blockTxns
    return (findTxns blocks, excludes `union` map blockHash blocks)
  where
    blockHash = headerHash . blockHeader
    firstScript :: Tx -> Script
    firstScript = either undefined id . S.decode . scriptOutput . head . txOut
    -- TODO: use Haskoin DataCarrier logic instead once released
    isDataCarrier (Script [OP_RETURN, OP_PUSHDATA _ _]) = True
    isDataCarrier _ = False

fetchRecentBlocks :: [BlockHash] -> IO [Block]
fetchRecentBlocks excludes = do
    let url = baseURL ++ "/status?q=getLastBlockHash"
    r <- get url
    let lastBlockHash = encodeUtf8 $ r ^. responseBody . key "lastblockhash" . _String
    fetchBlockchain (fromMaybe undefined $ hexToBlockHash lastBlockHash) excludes 6
  where
    fetchBlockchain :: BlockHash -> [BlockHash] -> Int -> IO [Block]
    fetchBlockchain tipHash excludes depth
        | depth < 1 = return []
        | tipHash `elem` excludes = return []
        | otherwise = do
            let url = concat [baseURL, "/rawblock/", B8.unpack $ blockHashToHex tipHash]
            putStrLn "fetching a block..."
            r <- get url
            let b = toBlock $ r ^. responseBody . key "rawblock" . _String
            prevBlocks <- fetchBlockchain (prevBlock (blockHeader b)) excludes (depth - 1)
            return $ b : prevBlocks
    toBlock :: T.Text -> Block
    toBlock = either undefined id . S.decode . fromMaybe undefined . decodeHex . encodeUtf8
