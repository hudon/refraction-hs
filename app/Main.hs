{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment
import System.IO
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Refraction as R

configFilename = ".refraction.yaml"

-- Specify options
data Options = Options  { optIsBob :: Bool, optIsAlice :: Bool, optIgnore :: Bool }

defaultOptions :: Options
defaultOptions = Options { optIsBob = False, optIsAlice = False, optIgnore = False }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['b'] ["isbob"]   (NoArg showBob)   "run in debug mode -- bob/advertiser",
    Option ['a'] ["isalice"] (NoArg showAlice) "run in debug mode -- alice/respondent",
    Option ['i'] ["ignore-validation"] (NoArg showIgnore) "ignore key validation (not recommended)"
  ]

showBob opt = do
    putStrLn "Running Refraction in debug mode as Bob (Advertiser)"
    return opt { optIsBob = True }

showAlice opt = do
    putStrLn "Running Refraction in debug mode as Alice (Respondent)"
    return opt { optIsAlice = True }

showIgnore opt = do
    putStrLn "WARNING: Ignoring key validation"
    return opt { optIgnore = True }

readOptions = do
  args <- getArgs
  let (actions, nonOpts, msgs) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optIsBob = isBob, optIsAlice = isAlice, optIgnore = ignoreValidation } = opts
  return (isBob, isAlice, ignoreValidation)

readConfig :: IO R.RefractionConfig
readConfig = either (error . show) id <$> decodeFileEither configFilename

touchConfig :: IO ()
touchConfig = doesFileExist configFilename >>= writeIfFalse
  where writeIfFalse False = writeFile configFilename "bitcoin:\n  network: testnet3\n  insightURL: https://testnet.blockexplorer.com"
        writeIfFalse _ = return ()

runRefraction :: Bool -> Bool -> Bool -> IO ()
runRefraction isBob isAlice ignoreValidation = do
    touchConfig
    config <- readConfig
    T.putStr "Please start Tor. Is Tor running? (y/n): "
    hFlush stdout
    _ <- T.getLine
    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine
    putStr "Enter destination address (Base58 encoded): "
    hFlush stdout
    pubkey <- T.getLine
    R.refract config isBob isAlice ignoreValidation prvkey pubkey

main = do
    (isBob, isAlice, ignoreValidation) <- readOptions
    runRefraction isBob isAlice ignoreValidation
