{-# LANGUAGE OverloadedStrings #-}
module Blockchain
    ( broadcast
    ) where

import qualified Data.ByteString as B
import Network.Haskoin.Crypto.Base58

broadcast :: B.ByteString -> IO ()
broadcast tx = undefined

utxos :: Address -> [(Tx, Int)]
utxos = undefined
