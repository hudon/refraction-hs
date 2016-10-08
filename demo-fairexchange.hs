import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import Control.Monad (guard, liftM)
import Data.ByteString.Char8 hiding (map, putStrLn)
import Data.Maybe
import Data.Serialize
import Generator
import Network.Haskoin.Constants
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction
import Network.Haskoin.Util

-- testnet address:moCvBdctTGGBwquWx647GvQWAsr4XBQBXh
-- run with `stack runhaskell demo-fairexchange-txs.hs`
-- use https://testnet.blockexplorer.com/tx/send to broadcast

main = do
    switchToTestnet3
    makeBobCommitTx
    makeAliceCommitTx

makeBobCommitTx = do
    let txhash = "TxHash \"02abfb785f8064d0ddf656101f09a6f1b53bfd08723ccb050eb32ba0c7a25054\""
        n = 1
        value = 100000
        -- hex scriptPubKey
        script = "76a9145457c1cbd45710c749b7aba24f9d9e97382893d588ac"
        -- wif prvkey
        p = "PrvKey \"cQrgind4kVZbZpfAVfZq8Nw6HcPBZyT2pktrn2t5dSS7H9aeNFmx\""
        secrets = [42, 56] :: [Integer]

    let utxo = toUTXO txhash n value script
        prv = read p
        pub = derivePubKey prv
        hashes = map (hash256 . encode) secrets

    let tx = either undefined id $ makeBobCommit [utxo] [prv] pub pub hashes
    putStrLn $ show tx

makeAliceCommitTx = do
    let txhash = "TxHash \"c74c08815ee65484db9fd6b82b036e36bb7a9b023f9698fc8bf9a7c3c8883403\""
        n = 1
        value = 100000
        -- hex scriptPubKey
        script = "76a9145457c1cbd45710c749b7aba24f9d9e97382893d588ac"
        -- wif prvkey
        p = "PrvKey \"cQrgind4kVZbZpfAVfZq8Nw6HcPBZyT2pktrn2t5dSS7H9aeNFmx\""
        secrets = [56, 123] :: [Integer]

    let utxo = toUTXO txhash n value script
        prv = read p
        pub = derivePubKey prv
        hashes = map (hash256 . encode) secrets

    let tx = either undefined id $ makeAliceCommit [utxo] [prv] pub pub hashes
    putStrLn $ show tx

toUTXO txhash n value script =
    let s = fromMaybe undefined . decodeHex $ pack script
        op = OutPoint { outPointHash=(read txhash), outPointIndex=n }
        to = TxOut { outValue=value, scriptOutput=s }
    in UTXO { _outPoint=op, _txOut=to }
