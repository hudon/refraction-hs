{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    ) where

import qualified Data.ByteString as B

makeSimpleTransaction :: String -> String -> B.ByteString
makeSimpleTransaction prvkey addr = undefined
