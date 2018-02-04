{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Discover.Types
    ( adFinder
    , tao
    , onionLengthWithoutTLD
    , Location
    , Nonce
    ) where

import Data.ByteString (ByteString)
import Data.Text
import Data.Word (Word64)

import Network.Refraction.BitcoinUtils

-- TODO(hudon): make this fee dynamic (set by user in config?)
-- Advertise fee
tao = 10000 :: SatoshiValue

--TODO(hudon): identifier could be unique per pool
adFinder = "RFRCTN14" :: Text
onionLengthWithoutTLD = 16 :: Int

type Location = ByteString
type Nonce = Word64
