module Network.Refraction.GeneratorSpec (spec) where

import Data.Either
import qualified Data.Serialize as S
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import qualified Network.Refraction.Generator as G
import Test.Hspec

spec :: Spec
spec = do
    describe "mkInput" $ do
        it "returns Left if non-decodable" $ do
            let txout = TxOut 5 (S.encode "foo")
                utxo = G.UTXO txout undefined
            G.mkInput utxo `shouldSatisfy` isLeft
