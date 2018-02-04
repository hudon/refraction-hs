{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Network.Refraction.Blockchain
    ( broadcastTx
    , findOPRETURNs
    , fetchSpentTxId
    , fetchTx
    , fetchTxWaitForRelay
    , fetchUTXOs
    , waitForTxToAddress
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (_String, key)
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (union)
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Network.Haskoin.Block
import Network.Haskoin.Crypto (Address, addrToBase58)
import Network.Haskoin.Script (Script(..), ScriptOp(..))
import Network.Haskoin.Transaction
import Network.Haskoin.Util (decodeHex)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Network.Wreq

import Network.Refraction.BitcoinUtils

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

broadcastTx :: Tx -> IO (Maybe TxHash)
broadcastTx tx = do
    putStrLn "Broadcasting"
    print tx
    let url = baseURL ++ "/tx/send"
    eresponse <- try $ post url (toJSON $ TxPayload tx)
    case eresponse of
        Left e -> print (e :: HttpException) >> return Nothing
        Right r -> processResponse r
  where
    processResponse r = do
        rjson <- asJSON r
        let txid = broadcastTxid $ rjson ^. responseBody
        print txid
        return $ Just txid

-- |Attempts to fetch the TX. Return Nothing if 404, can throw exception if other error occurs
fetchTx :: TxHash -> IO (Maybe Tx)
fetchTx txhash = do
    let url = baseURL ++ "/rawtx/" ++ B8.unpack (txHashToHex txhash)
    eres <- try $ get url
    case eres of
      Left e -> handleErr e
      Right r -> do
        r <- asJSON =<< get url
        return . Just . rawtx $ r ^. responseBody
  where
    handleErr e@(HttpExceptionRequest _ (StatusCodeException s _))
      | s ^. responseStatus ^. statusCode == 404 = return Nothing
      | otherwise = throwIO e

-- TODO(hudon): add timeout
fetchTxWaitForRelay :: TxHash -> IO Tx
fetchTxWaitForRelay th = do
    putStrLn "Waiting for round split tx relay..."
    fetchTx th >>= maybe loop return
  where loop = threadDelay 30000000 >> fetchTxWaitForRelay th

fetchSpentTxId :: TxHash -> Int -> IO (Maybe TxHash)
fetchSpentTxId txhash index = do
    let url = baseURL ++ "/tx/" ++ B8.unpack (txHashToHex txhash)
    r <- asJSON =<< get url
    let result = r ^. responseBody
    let spentTxId = flip parseMaybe result $ \obj -> do outs <- obj .: "vout"
                                                        (outs !! index) .: "spentTxId"
    return $ maybe Nothing (hexToTxHash . encodeUtf8) spentTxId

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

-- Fetches UTXOs for the given address and filters for only the UTXOs with positive amounts
fetchUTXOs :: Address -> IO [UTXO]
fetchUTXOs addr = do
    let url = baseURL ++ "/addr/" ++ B8.unpack (addrToBase58 addr) ++ "/utxo"
    r <- asJSON =<< get url
    return . filter hasPositiveAmount . map toUTXO $ r ^. responseBody
  where
    hasPositiveAmount utxo = coinValue utxo > 0

findOPRETURNs :: [BlockHash] -> IO ([Script], [BlockHash])
findOPRETURNs excludes = do
    blocks <- fetchRecentBlocks excludes
    let findTxns = filter isDataCarrier . map firstScript . concat . map blockTxns
    return (findTxns blocks, excludes `union` map blockHash blocks)
  where
    blockHash = headerHash . blockHeader
    firstScript :: Tx -> Script
    -- TODO(hudon): what if the ad is not in the first output?...
    firstScript = either undefined id . S.decode . scriptOutput . head . txOut
    -- TODO: use Haskoin DataCarrier logic instead once released
    isDataCarrier (Script [OP_RETURN, OP_PUSHDATA _ _]) = True
    isDataCarrier _ = False

fetchRecentBlocks :: [BlockHash] -> IO [Block]
fetchRecentBlocks excludes = do
    let url = baseURL ++ "/status?q=getLastBlockHash"
    r <- get url
    let lastBlockHash = encodeUtf8 $ r ^. responseBody . key "lastblockhash" . _String
    fetchBlockchain (fromMaybe undefined $ hexToBlockHash lastBlockHash) excludes 5
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

-- Waits until the given address receives funds, and returns the funding transaction
waitForTxToAddress :: Address -> IO Tx
waitForTxToAddress = undefined
