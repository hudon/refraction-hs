{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    ) where

import qualified Data.ByteString as B
import Network.Haskoin.Transaction.Types

makeSimpleTransaction :: [OutPoint] -> [(ByteString, Word64)] -> Either String Tx
makeSimpleTransaction prvkey addr = undefined
