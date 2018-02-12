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
import qualified Network.Refraction as R
import qualified Network.Refraction.BitcoinUtils as BU
import qualified Network.Refraction.Tor as BT

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

untilM :: Monad m => m Bool -> m a -> m ()
untilM p a = do
  done <- p
  if done
    then return ()
    else a >> untilM p a

runRefraction :: Bool -> Bool -> Bool -> IO ()
runRefraction isBob isAlice ignoreValidation = do
    touchConfig
    config <- readConfig

    let askForTor = T.putStr "Tor daemon not found. Start Tor and press enter to try again..." >> hFlush stdout >> T.getLine
    untilM BT.isTorUp askForTor

    T.putStr "Enter source private key (WIF encoded): "
    hFlush stdout
    prvkey <- T.getLine

    putStr "Enter destination address for mixed coins (Base58 encoded): "
    hFlush stdout
    endAddr <- T.getLine

    putStr "Enter refund address for non-mixed coins that need to be refunded (Base58 encoded):"
    hFlush stdout
    refundAddr <- T.getLine

    -- CURRENTLY UNUSED:
    --(_, startAddr) <- BU.makeTextKeyPair
    --putStr "Send funds to mix in 1 transaction to this address to begin mixing: "
    --T.putStrLn startAddr
    --hFlush stdout

    R.refract config isBob isAlice ignoreValidation prvkey endAddr refundAddr

main = do
    (isBob, isAlice, ignoreValidation) <- readOptions
    runRefraction isBob isAlice ignoreValidation
