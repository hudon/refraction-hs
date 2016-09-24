{-# LANGUAGE OverloadedStrings #-}
module PeerToPeer
    ( Msg
    , startServer
    , startClient
    ) where


import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Monad (liftM)
import Network (PortNumber)
import Network.Socket
import System.IO

type Msg = String

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    clientHelloMsg <- hGetLine hdl
    hPutStrLn hdl "hi"
    clientByeMsg <- liftM init (hGetLine hdl)
    writeChan chan $ "client says: " ++ clientHelloMsg
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

runClient :: HostName             -- ^ Remote hostname, or localhost
          -> String               -- ^ Port number or name; 4242 is default
          -> IO ()
runClient hostname port =
    do -- Look up the hostname and port.  Either raises an exception
       -- or returns a nonempty list.  First element in that list
       -- is supposed to be the best option.
       addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
       let serveraddr = head addrinfos

       -- Establish a socket for communication
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol

       -- Connect to server
       connect sock (addrAddress serveraddr)

       -- Make a Handle out of it for convenience
       h <- socketToHandle sock ReadWriteMode

       hSetBuffering h NoBuffering

       hPutStrLn h "hello!"
       bobHiMsg <- liftM init (hGetLine h)
       hPutStrLn h "bye"
       hClose h

startClient :: IO ()
startClient = runClient "127.0.0.1" "4242"
