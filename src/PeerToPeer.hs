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
    aliceHelloMsg <- liftM init (hGetLine hdl)
    hPutStrLn hdl "hi"
    aliceByeMsg <- liftM init (hGetLine hdl)
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

       hPutStrLn h "hello"
       bobHiMsg <- liftM init (hGetLine h)
       hPutStrLn h "bye"
       hClose h

startClient :: IO ()
startClient = runClient "127.0.0.1" "4242"
