{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Tor
    ( secureConnect
    , makeHiddenService
    ) where

import Control.Concurrent (forkIO)
import Data.Base32String (toText)
import Data.ByteString (ByteString)
import Data.Text (append, Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.Anonymous.Tor as Tor
import Network (PortNumber)
import Network.Socket
import qualified Network.Socks5.Types as SocksT

-- | Assumes at least one of these two ports is available for control (tor must be running)
whichControlPort :: IO Integer
whichControlPort = do
    let ports = [9051, 9151]
    availability <- mapM Tor.isAvailable ports
    return . fst . head . filter ((== Tor.Available) . snd) $ zip ports availability

-- TODO(hudon): better failure if tor is not running... now it's a cryptic (empty list) error
makeHiddenService :: PortNumber -> (ByteString -> IO ()) -> IO ()
makeHiddenService privatePort f = do
    torPort <- whichControlPort
    Tor.withSession torPort $ \controlSock -> do
        onion <- Tor.mapOnion controlSock 80 (toInteger privatePort) False Nothing
        f . encodeUtf8 $ append (toText onion) ".onion"

secureConnect :: ByteString -> (Socket -> IO ()) -> IO ()
secureConnect addr f = do
    torPort <- whichControlPort
    Tor.withSession torPort $ \controlSock -> Tor.connect' controlSock destination f
  where
    destination = SocksT.SocksAddress (SocksT.SocksAddrDomainName addr) 80
