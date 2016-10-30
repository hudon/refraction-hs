{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.PeerToPeer
    ( unsecureSend
    , Msg
    , sendMessage
    , startServer
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Monad (liftM)
import Data.ByteString (ByteString, hGetLine)
import Data.ByteString.Char8 (hPutStrLn)
import Data.Text (append)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network (PortNumber)
import Network.Socket
import System.IO hiding (hGetLine, hPutStrLn)

type Msg = ByteString

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    clientMsg <- hGetLine hdl
    writeChan chan clientMsg
    hClose hdl

serverLoop :: Socket -> Chan Msg -> IO ()
serverLoop sock chan = do
    conn <- accept sock
    forkIO $ runConn conn chan
    serverLoop sock chan

startServer :: PortNumber -> IO (Chan Msg)
startServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    chan <- newChan
    forkIO $ serverLoop sock chan
    return chan

sendMessage :: Msg -> Socket -> IO ()
sendMessage m sock = do
     h <- socketToHandle sock ReadWriteMode
     hSetBuffering h NoBuffering
     hPutStrLn h m
     hClose h

unsecureConnect :: HostName -> String -> (Socket -> IO ()) ->  IO ()
unsecureConnect hostname port f = do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    connect sock (addrAddress serveraddr)
    f sock

unsecureSend :: Bool -> Msg -> IO ()
unsecureSend isBob m = do
    let port = if isBob then "4243" else "4242"
    unsecureConnect "127.0.0.1" port (sendMessage m)
