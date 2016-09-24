{-# LANGUAGE OverloadedStrings #-}
module Tor
    ( withinSession
    ) where

import Control.Concurrent (forkIO)
import Data.Base32String (toText)
import Data.ByteString (ByteString)
import Data.Text (append, Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.Anonymous.Tor as Tor
import qualified Network.Simple.TCP as NST
import Network.Socket
import qualified Network.Socket.Splice as Splice
import System.IO (IOMode(ReadWriteMode))

-- |Assumes at least one of these two ports is available for control (tor must be running)
whichControlPort :: IO Integer
whichControlPort = do
    let ports = [9051, 9151]
    availability <- mapM Tor.isAvailable ports
    return . fst . head . filter ((== Tor.Available) . snd) $ zip ports availability

withinSession :: (ByteString -> IO ()) -> IO ()
withinSession f = do
    torPort <- whichControlPort
    Tor.withSession torPort $ \controlSocket -> do
        onion <- Tor.accept controlSocket 80 Nothing (newConnection 4242)
        putStrLn $ "hidden service descriptor: " ++ show onion
        f . encodeUtf8 $ append (toText onion) ".onion"
  where
    newConnection privatePort sPublic =
        NST.connect "127.0.0.1" (show privatePort) $ \(sPrivate, addr) -> spliceSockets sPublic sPrivate
    spliceSockets sLhs sRhs = do
       hLhs <- socketToHandle sLhs ReadWriteMode
       hRhs <- socketToHandle sRhs ReadWriteMode
       _ <- forkIO $ Splice.splice 1024 (sLhs, Just hLhs) (sRhs, Just hRhs)
       Splice.splice 1024 (sRhs, Just hRhs) (sLhs, Just hLhs)
