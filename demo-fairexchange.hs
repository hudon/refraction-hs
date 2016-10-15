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
    --makeBobCommitTx
    --makeAliceCommitTx
    -- Once you have the hex-encoded redeem scripts from the above commit txs as well as the
    -- tx info, fill in the claim tx inputs below and re-run this script
    makeBobClaimTx
    makeAliceClaimTx

makeBobCommitTx = do
    let txhash = "TxHash \"6eee3902841753298e98a556f02425ea2b75c390cb4ff7e3d25bf9ebdab77db8\""
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
        hashes = map (doubleHash256 . encode) secrets

    let (tx, redeem) = either undefined id $ makeBobCommit [utxo] [prv] pub pub hashes
    putStrLn "Bob commit tx:"
    putStrLn $ show tx
    putStrLn "Redeem script for Bob's commit:"
    putStrLn $ unpack $ encodeHex $ encode redeem
    putStrLn $ show $ hash160 $ getHash256 $ hash256 $ encode redeem

makeAliceCommitTx = do
    let txhash = "TxHash \"c58f3a355b1bece578d757b5597c25a42a0b4ec43d8c8a4af2048319fb9543da\""
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
        hashes = map (doubleHash256 . encode) secrets

    let (tx, redeem) = either undefined id $ makeAliceCommit [utxo] [prv] pub pub hashes
    putStrLn "Alice commit tx:"
    putStrLn $ show tx
    putStrLn "Redeem script for Alice's commit:"
    putStrLn $ unpack $ encodeHex $ encode redeem
    putStrLn $ show $ hash160 $ getHash256 $ hash256 $ encode redeem

makeBobClaimTx = do
    let txhash = "TxHash \"69f72c3105704a421584cc5eb27dcf4fce20cc6820012eaa22a67cb2fc9ac352\""
        n = 0
        value = 90000
        -- hex scriptPubKey
        script = "a914d7029e2efa92ea7257159337250e9ffa09fb633187"
        -- wif prvkey
        p = "PrvKey \"cQrgind4kVZbZpfAVfZq8Nw6HcPBZyT2pktrn2t5dSS7H9aeNFmx\""
        secrets = [56, 123] :: [Integer]
        redeemHex = "630500000186a0b1752103f7653b7d6db6dbb4139ef05484a92e620312a822105c526c4f91b65e76f5ad15ac672103f7653b7d6db6dbb4139ef05484a92e620312a822105c526c4f91b65e76f5ad15adaa200880e9d991ac4931bf64bb2809100af425ab761e081b349d54cfb8f2ec1dbd2788aa2032115336ddd57f8423974294d75418a27128aca19c71162287e457aab85f31658768"

    let utxo = toUTXO txhash n value script
        prv = read p
        pub = derivePubKey prv
        -- TODO maybe the other generators should take [Integer] rather than bytestrings
        --hashes = map encode secrets
        redeem = either undefined id . decode . fromMaybe undefined . decodeHex $ pack redeemHex

    let tx = either undefined id $ makeBobClaim [utxo] [prv] redeem secrets pub
    putStrLn "Bob claim tx:"
    putStrLn $ show tx


makeAliceClaimTx = do
    let txhash = "TxHash \"ab562be2be6255acbf5e595f29b3da730b7f5e5961bc89472c965aa9184168ed\""
        n = 0
        value = 90000
        -- hex scriptPubKey
        script = "a91449cfe9e4fe08a9ddf5859aca9bb84d916cf1a5ae87"
        -- wif prvkey
        p = "PrvKey \"cQrgind4kVZbZpfAVfZq8Nw6HcPBZyT2pktrn2t5dSS7H9aeNFmx\""
        secrets = [56, 123] :: [Integer]
        redeemHex = "630500000186a0b1752103f7653b7d6db6dbb4139ef05484a92e620312a822105c526c4f91b65e76f5ad15ac672103f7653b7d6db6dbb4139ef05484a92e620312a822105c526c4f91b65e76f5ad15adaa7620da149891917e58f3808d3c0fbbe5385cfa33f2cc93e4bfd234d2817fdb82da0e877c200880e9d991ac4931bf64bb2809100af425ab761e081b349d54cfb8f2ec1dbd27879b68"

    let utxo = toUTXO txhash n value script
        prv = read p
        pub = derivePubKey prv
        --hashes = map encode secrets
        redeem = either undefined id . decode . fromMaybe undefined . decodeHex $ pack redeemHex

    let tx = either undefined id $ makeAliceClaim [utxo] [prv] redeem (Prelude.head secrets) pub
    putStrLn "Alice claim tx"
    putStrLn $ show tx


toUTXO txhash n value script =
    let s = fromMaybe undefined . decodeHex $ pack script
        op = OutPoint { outPointHash=(read txhash), outPointIndex=n }
        to = TxOut { outValue=value, scriptOutput=s }
    in UTXO { _outPoint=op, _txOut=to }
