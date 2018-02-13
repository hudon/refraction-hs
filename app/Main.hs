module Main where

import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.IO
import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Yaml
import qualified Network.Refraction as R
import qualified Network.Refraction.BitcoinUtils as BU
import qualified Network.Refraction.Tor as BT

configFilename = ".refraction.yaml"

data Flag
  = IsBob
  | IsAlice
  | Help
  deriving Eq

optionDescs :: [OptDescr Flag]
optionDescs = [
    Option ['b'] ["isbob"]   (NoArg IsBob)   "run in debug mode -- bob/advertiser",
    Option ['a'] ["isalice"] (NoArg IsAlice) "run in debug mode -- alice/respondent",
    Option ['h'] ["help"] (NoArg Help) "prints this help message"
  ]
-- TODO(hudon): add support for bech32
header = "Usage: refraction [OPTION...] SOURCE-PRVKEY-WIF DESTINATION-BASE58 REFUND-BASE58"

readOptions = do
  initialArgs <- getArgs
  return $ case getOpt RequireOrder optionDescs initialArgs of
    (o, a, []) -> Right (o, a)
    (_, _, errs) -> Left errs

readConfig :: IO R.RefractionConfig
readConfig = either (error . show) id <$> decodeFileEither configFilename

touchConfig :: IO ()
touchConfig = doesFileExist configFilename >>= writeIfFalse
  where writeIfFalse False = writeFile configFilename "bitcoin:\n  network: testnet3\n  insightURL: https://testnet.blockexplorer.com"
        writeIfFalse _ = return ()

throwIfTrue :: IO Bool -> String -> IO ()
throwIfTrue check err = check >>= \c -> when c (ioError (userError err))

main = do
  isTorUp <- BT.isTorUp
  when (not isTorUp) $ ioError (userError "Tor daemon not found. Start Tor and try again")

  readOptions >>= \o -> case o of
    Left errs -> usageFailure errs
    Right (opts, args) -> do
      when (Help `elem` opts) $ putStrLn (usageInfo header optionDescs) >> exitSuccess
      let isBob = IsBob `elem` opts
          isAlice = IsAlice `elem` opts

      when (length args /= 3) $ usageFailure []
      let (srcPrv:destAddr:refundAddr:[]) = args

      touchConfig
      config <- readConfig

      R.refract config isBob isAlice srcPrv destAddr refundAddr
  where
    usageFailure errs = ioError $ userError (concat errs ++ usageInfo header optionDescs)
