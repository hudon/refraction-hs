{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.IO


main :: IO ()
main = do
    putStr "Enter source private key: "
    hFlush stdout
    prvkey <- getLine
    putStr "Enter destination address: "
    hFlush stdout
    pubkey <- getLine
    putStr "Your prvkey: "
    hFlush stdout
    putStrLn prvkey
