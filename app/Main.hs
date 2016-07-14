{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import qualified Refraction as R


main :: IO ()
main = do
    putStr "Enter source private key: "
    hFlush stdout
    prvkey <- getLine
    putStr "Enter destination address: "
    hFlush stdout
    pubkey <- getLine
    R.refract prvkey pubkey
