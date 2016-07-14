{-# LANGUAGE OverloadedStrings #-}
module Blockchain
    ( broadcast
    ) where

import qualified Data.ByteString as B

broadcast :: B.ByteString -> IO ()
broadcast tx = undefined
