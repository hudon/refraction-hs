{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO
import qualified Data.Text.IO as T
import qualified Refraction as R


main :: IO ()
main = do
    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine
    putStr "Enter destination address (Base58 encoded): "
    hFlush stdout
    pubkey <- T.getLine
    R.refract prvkey pubkey
