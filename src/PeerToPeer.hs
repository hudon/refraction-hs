{-# LANGUAGE OverloadedStrings #-}
module PeerToPeer
    ( startServer
    , startClient
    ) where


import Control.Monad (liftM)
import Network.Socket
import System.IO

runConn :: (Socket, SockAddr) -> Int -> IO ()
runConn (sock, _) msgNum = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    name <- liftM init (hGetLine hdl)
    hPutStrLn hdl "hi"
    name <- liftM init (hGetLine hdl)

    hClose hdl

mainLoop :: Socket -> Int -> IO ()
mainLoop sock msgNum = do
  conn <- accept sock
  runConn conn msgNum

startServer :: IO ()
startServer = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock 0

startClient :: IO ()
startClient = undefined
