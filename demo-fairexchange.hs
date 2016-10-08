import Control.Monad.CryptoRandom (crandomRs)
import Crypto.Random.DRBG (CtrDRBG, newGenIO)
import Control.Monad (guard, liftM)
import Data.ByteString.Char8 hiding (putStrLn)
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

    let txhash = "TxHash \"8761f25393a6f3bdf3d57c39b380a2f1cbc92382a28e704c730bfdfd7a429012\""
        n = 1
        value = 100000
        -- hex scriptSig
        script = "76a9145457c1cbd45710c749b7aba24f9d9e97382893d588ac"
        -- wif prvkey
        p = "PrvKey \"cQrgind4kVZbZpfAVfZq8Nw6HcPBZyT2pktrn2t5dSS7H9aeNFmx\""
        secret = 56 :: Integer

    let utxo = toUTXO txhash n value script
        prv = read p
        pub = derivePubKey prv
        hash = hash256 $ encode secret

    let tx = either undefined id $ makeBobCommit [utxo] [prv] pub pub [hash]
    putStrLn $ show tx

toUTXO txhash n value script =
    let s = fromMaybe undefined . decodeHex $ pack script
        op = OutPoint { outPointHash=(read txhash), outPointIndex=n }
        to = TxOut { outValue=value, scriptOutput=s }
    in UTXO { _outPoint=op, _txOut=to }
