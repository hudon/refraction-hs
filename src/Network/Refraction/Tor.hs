{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Tor
    (isTorUp
    , makeHiddenService
    , secureConnect
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
whichControlPort :: IO (Maybe Integer)
whichControlPort = do
    let ports = [9051, 9151]
    availability <- mapM Tor.isAvailable ports
    return . selectFirst . filter ((== Tor.Available) . snd) $ zip ports availability
  where
    selectFirst [] = Nothing
    selectFirst (x:_) = Just $ fst x

-- TODO(hudon): better failure if tor is not running... now it's a cryptic (empty list) error
makeHiddenService :: PortNumber -> (ByteString -> IO ()) -> IO (Either String ())
makeHiddenService privatePort f = do
    t <- whichControlPort
    case t of
      Nothing -> return $ Left "Tor control port was unavailable. Tor may not be running."
      Just torPort -> Tor.withSession torPort $ \controlSock -> do
        -- Port 80 is "virtual port" and not an actual port on the host
        onion <- Tor.mapOnion controlSock 80 (toInteger privatePort) False Nothing
        f . encodeUtf8 $ append (toText onion) ".onion"
        return $ Right ()

secureConnect :: ByteString -> (Socket -> IO ()) -> IO (Either String ())
secureConnect addr f = do
    t <- whichControlPort
    case t of
      Nothing -> return $ Left "Tor control port was unavailable. Tor may not be running."
      Just torPort -> do
        Tor.withSession torPort $ \controlSock -> Tor.connect' controlSock destination f
        return $ Right ()
  where
    destination = SocksT.SocksAddress (SocksT.SocksAddrDomainName addr) 80

isTorUp :: IO Bool
isTorUp = let maybeToBool Nothing = return False
              maybeToBool _ = return True
          in whichControlPort >>= maybeToBool
