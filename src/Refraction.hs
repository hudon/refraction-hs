{-# LANGUAGE OverloadedStrings #-}
module Refraction
    ( isValidPrivateKey
    , refract
    ) where

isValidPrivateKey :: String -> Bool
isValidPrivateKey prv = True

handleBadPrvkey :: String -> IO ()
handleBadPrvkey prv = putStrLn "ERROR: private key is not valid"

isValidAddress :: String -> Bool
isValidAddress addr = True

handleBadAddress :: String -> IO()
handleBadAddress addr = putStrLn "ERROR: address is not valid"

refract :: String -> String -> IO ()
refract prv addr = do
    putStrLn "INFO: Starting refraction"
    if not (isValidPrivateKey prv) then handleBadPrvkey prv else do
        if not (isValidAddress addr) then handleBadAddress addr else do
            return ()
