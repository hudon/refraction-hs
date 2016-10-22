module Network.Refraction.GeneratorSpec (spec) where

import Data.Either
import qualified Data.Serialize as S
import Data.Word
import Network.Haskoin.Constants
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import qualified Network.Refraction.Generator as G
import Test.Hspec

spec :: Spec
spec = do
    describe "mkInput" $ do
        it "returns Left if non-decodable" $ do
            let txout = TxOut preAdValue $ S.encode "foo" -- bad output script
                badUTXO = G.UTXO txout undefined
            G.mkInput badUTXO `shouldSatisfy` isLeft
    describe "makeAdTransaction" $ do
        it "returns change to signatory" $ do
            let isReturnedChange = isChange . decodeOutputBS . scriptOutput
                isChange (Right (PayPKHash addr)) = addr == advertiserAddress
                isChange _ = False
            any isReturnedChange (txOut prepareAdTx) `shouldBe` True
        it "has 2 outputs" $ do
            length (txOut prepareAdTx) `shouldBe` 2
    describe "makePairRequest" $ do
        it "returns change to signatory" $ do
            let isReturnedChange = isChange . decodeOutputBS . scriptOutput
                isChange (Right (PayPKHash addr)) = addr == advertiserAddress
                isChange _ = False
            any isReturnedChange (txOut preparePairRequest) `shouldBe` True
        it "has 2 outputs" $ do
            length (txOut preparePairRequest) `shouldBe` 2


-- same prvkey as in demo-fairexchange.hs but on mainnet
advertiserPrvKey = read "PrvKey \"KzVhFsdDKRsLQPBu7Fkhm4S2fP5muXMLkikPfcRa8Kn72QTDuTDH\""

advertiserAddress = pubKeyAddr $ derivePubKey advertiserPrvKey

preAdValue = 100000000 :: G.SatoshiValue -- 1 btc
preAdTxHash = read "TxHash \"aa89f9878323075e109efcbd44c7804db4bea9c85910d002d888b9fa70087570\""
preAdUTXO =
    let txout = TxOut preAdValue $ encodeOutputBS $ PayPKHash advertiserAddress
        op = OutPoint preAdTxHash 1
    in G.UTXO txout op

adFee = 10000000 :: G.SatoshiValue -- 0.1 btc

prepareAdTx :: Tx
prepareAdTx =
    let utxos = [preAdUTXO]
        loc = S.encode "foo"
        nonce = 23498 :: Word64
        adFee = adFee
        ref = S.encode "baz"
        Right tx = G.makeAdTransaction utxos advertiserPrvKey loc nonce adFee ref
    in tx

preparePairRequest :: Tx
preparePairRequest =
    let utxos = [preAdUTXO]
        loc = S.encode "foo"
        nonce = 23498 :: Word64
        adFee = adFee
        ref = S.encode "baz"
        Right tx = G.makePairRequest utxos advertiserPrvKey (nonce, nonce) adFee ref
    in tx
