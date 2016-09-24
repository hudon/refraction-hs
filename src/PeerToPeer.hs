{-# LANGUAGE OverloadedStrings #-}
module PeerToPeer
    ( Msg
    , sendMessage
    , startServer
    ) where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Network (PortNumber)
import Network.Socket
import System.IO

type Msg = String

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    clientMsg <- hGetLine hdl
    hPutStrLn hdl "hi"
    writeChan chan $ "client says: " ++ clientMsg
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
     bobHiMsg <- hGetLine h
     putStrLn $ "bob says " ++ bobHiMsg
     hClose h
