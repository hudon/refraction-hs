{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Environment
import System.IO
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Yaml
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Refraction as R


-- Specify config file format
data RefractionConfig = RefractionConfig { bitcoin :: BitcoinConfig } deriving Show

data BitcoinConfig = BitcoinConfig { network :: Text } deriving Show

instance FromJSON RefractionConfig where
    parseJSON (Object m) = RefractionConfig <$> m .: "bitcoin"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON BitcoinConfig where
    parseJSON (Object m) = BitcoinConfig <$> m .: "network"
    parseJSON x = fail ("not an object: " ++ show x)

readConfig :: IO RefractionConfig
readConfig = either (error . show) id <$> decodeFileEither ".refraction.yaml"


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


runRefraction :: Bool -> Bool -> IO ()
runRefraction isBob ignoreValidation = do
    config <- readConfig
    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine
    putStr "Enter destination address (Base58 encoded): "
    hFlush stdout
    pubkey <- T.getLine
    R.refract (network (bitcoin config)) isBob ignoreValidation prvkey pubkey

main = do
    (isBob, ignoreValidation) <- readOptions
    runRefraction isBob ignoreValidation
