module Network.Refraction.FairExchange.Bob
    ( runBob
    ) where

import Control.Concurrent.Chan (Chan, readChan)
import Control.Monad (liftM)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.Haskoin.Crypto
import Network.Refraction.FairExchange.Types
-- TODO(hudon): doesn't make sense to depend on Discover for location type
import Network.Refraction.Discover (Location)
import Network.Refraction.PeerToPeer (Msg, sendMessage)
import Network.Refraction.Tor(secureConnect)

-- TODO(hudon): for now we pay back to the initial keypair for the claim tx
-- but in the future claim should go to a completely new pubkey

runBob :: Chan Msg -> KeyPair -> KeyPair -> [Secret] -> Location -> IO ()
runBob chan keypair refundKeypair mySecrets theirLocation = do
    putStrLn "Running Bob fair exchange protocol..."
    setupBobSecrets chan keypair refundKeypair mySecrets theirLocation
    bobCommit keypair theirLocation
    bobClaim keypair

setupBobSecrets :: Chan Msg -> KeyPair -> KeyPair -> [Secret] -> Location -> IO ()
setupBobSecrets chan (_, bPub) (_, refundPub) mySecrets theirLocation = do
    aliceKeys <- liftM (fromMaybe undefined . A.decode) $ readChan chan
    let aliceSecrets = aSecrets aliceKeys
        bobHashes = map hashAndEncode mySecrets
        sums = zipWith (+) aliceSecrets mySecrets
        sumHashes = map hashAndEncode sums
    send $ BobKeyMessage bPub refundPub bobHashes sumHashes
    indices <- liftM (fromMaybe undefined . A.decode) $ readChan chan
    send $ map ((!!) mySecrets) indices
    return ()
  where
    hashAndEncode = bsToHexText . S.encodeLazy . doubleHash256 . S.encode
    send x = secureConnect theirLocation $ sendMessage (A.encode x)

bsToHexText :: BL.ByteString -> Text
bsToHexText = decodeUtf8 . B16.encode

bobCommit = undefined

bobClaim = undefined
