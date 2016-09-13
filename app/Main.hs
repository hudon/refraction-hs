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
data Options = Options  { optIsBob :: Bool, optIgnore :: Bool }

defaultOptions :: Options
defaultOptions = Options { optIsBob = False, optIgnore = False }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['b'] ["isbob"]   (NoArg showBob)   "run as Bob (server)",
    Option ['i'] ["ignore-validation"] (NoArg showIgnore) "ignore key validation (not recommended)"
  ]

showBob opt = do
    putStrLn "Running Refraction as Bob (server)"
    return opt { optIsBob = True }

showIgnore opt = do
    putStrLn "WARNING: Ignoring key validation"
    return opt { optIgnore = True }

readOptions = do
  args <- getArgs
  let (actions, nonOpts, msgs) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optIsBob = isBob, optIgnore = ignoreValidation } = opts
  return (isBob, ignoreValidation)

readConfig :: IO R.RefractionConfig
readConfig = either (error . show) id <$> decodeFileEither configFilename

touchConfig :: IO ()
touchConfig = doesFileExist configFilename >>= writeIfFalse
  where writeIfFalse False = writeFile configFilename "bitcoin:\n  network: testnet3\n  insightURL: https://testnet.blockexplorer.com"
        writeIfFalse _ = return ()

runRefraction :: Bool -> Bool -> IO ()
runRefraction isBob ignoreValidation = do
    touchConfig
    config <- readConfig
    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine
    putStr "Enter destination address (Base58 encoded): "
    hFlush stdout
    pubkey <- T.getLine
    R.refract config isBob ignoreValidation prvkey pubkey

main = do
    (isBob, ignoreValidation) <- readOptions
    runRefraction isBob ignoreValidation
