{-# LANGUAGE OverloadedStrings #-}
module Refraction
    ( isValidPrivateKey
    , isValidAddress
    , refract
    ) where

import qualified PeerToPeer as P2P
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as TE
import qualified Network.Haskoin.Crypto as C

isValidPrivateKey :: T.Text -> Bool
isValidPrivateKey prv =
    case C.fromWif (TE.encodeUtf8 prv) of
        Nothing -> False
        Just _ -> True

handleBadPrvkey :: T.Text -> IO ()
handleBadPrvkey prv = putStrLn "ERROR: private key is not valid"

isValidAddress :: T.Text -> Bool
isValidAddress addr = case C.base58ToAddr (TE.encodeUtf8 addr) of
    Nothing -> False
    Just _ -> True

handleBadAddress :: T.Text -> IO()
handleBadAddress addr = putStrLn "ERROR: address is not valid"

refract :: T.Text -> Bool -> T.Text -> T.Text -> IO ()
refract network isBob prv addr = do
    TI.putStrLn $ T.append "INFO: Starting refraction on " network
    case () of
      _ | not (isValidPrivateKey prv) -> handleBadPrvkey prv
        | not (isValidAddress addr) -> handleBadAddress addr
        | isBob -> P2P.startServer
        | otherwise -> P2P.startClient
