{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    , makeAdData
    , makeAdTransaction
    , makeAliceCommit
    , makeBobCommit
    , SatoshiValue
    , UTXO(..)
    ) where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.Serialize as S
import Network.Haskoin.Crypto (Address(..), getHash256, hash160, hash256, Hash256, PrvKey, PubKey)
import Network.Haskoin.Script (decodeOutputBS, opPushData, Script(..), ScriptOp(..), ScriptOutput(..), SigHash(..))
import qualified Network.Haskoin.Transaction as T

type SatoshiValue = Word64

data UTXO = UTXO {
      _txOut :: T.TxOut
    , _outPoint :: T.OutPoint
} deriving (Show)

instance T.Coin UTXO where
    coinValue =  T.outValue . _txOut

-- Default transaction fee in satoshis
dTxFee = 10000

-- |Takes a UTXO and returns a SigInput that can be used to sign a Tx
mkInput :: UTXO -> T.SigInput
mkInput utxo = T.SigInput pso (_outPoint utxo) (SigAll False) Nothing
  where pso = either undefined id . decodeOutputBS . T.scriptOutput $ _txOut utxo

-- |Our miner fee grows linearly with the number of inputs
calculateAmount :: [UTXO] -> SatoshiValue
calculateAmount = foldl sumVal 0
  where
    sumVal v utxo = v + T.coinValue utxo - dTxFee

-- |Takes a list of utxos and associated private keys and pays to an address
makeSimpleTransaction :: [UTXO] -> [PrvKey] -> Address -> Either String T.Tx
makeSimpleTransaction utxos prvkeys addr =
    let tx = either undefined id $ T.buildTx (map _outPoint utxos) [(PayPKHash addr, calculateAmount utxos)]
        in T.signTx tx (map mkInput utxos) prvkeys

makeAliceCommit :: [UTXO] -> [PrvKey] -> PubKey -> PubKey -> [Hash256] -> Either String T.Tx
makeAliceCommit utxos prvkeys aPubkey bPubkey bHashes =
    let redeemScript = makeAliceCommitRedeem aPubkey bPubkey bHashes 100000
    in signP2SH utxos prvkeys redeemScript

makeBobCommit :: [UTXO] -> [PrvKey] -> PubKey -> PubKey -> [Hash256] -> Either String T.Tx
makeBobCommit utxos prvkeys aPubkey bPubkey bHashes =
    let redeemScript = makeBobCommitRedeem aPubkey bPubkey bHashes 100000
    in signP2SH utxos prvkeys redeemScript

-- TODO put this is haskoin
scriptAddrNonStd :: Script -> Address
scriptAddrNonStd = ScriptAddress . hash160 . getHash256 . hash256 . S.encode

signP2SH :: [UTXO] -> [PrvKey] -> Script -> Either String T.Tx
signP2SH utxos prvkeys redeemScript =
    let scriptOut = PayScriptHash (scriptAddrNonStd redeemScript)
        tx = either undefined id $ T.buildTx (map _outPoint utxos) [(scriptOut, calculateAmount utxos)]
    in T.signTx tx (map mkInput utxos) prvkeys

makeAliceCommitRedeem :: PubKey -> PubKey -> [Hash256] -> Integer -> Script
makeAliceCommitRedeem aPubkey bPubkey sumHashes locktime =
    Script $ [ OP_IF
             -- OP_NOP2 is OP_CHECKLOCKTIMEVERIFY (CLTV)
             -- TODO update haskoin with cltv op
             , opPushData (S.encode locktime), OP_NOP2, OP_DROP
             , opPushData (S.encode aPubkey), OP_CHECKSIG
             , OP_ELSE
             , opPushData (S.encode bPubkey), OP_CHECKSIGVERIFY
             ] ++
             concat (map (\hash -> [OP_HASH256, opPushData (S.encode hash), OP_EQUALVERIFY]) (init sumHashes)) ++
             [ OP_HASH256, opPushData $ S.encode (last sumHashes), OP_EQUAL ] ++
             [ OP_ENDIF ]

makeBobCommitRedeem :: PubKey -> PubKey -> [Hash256] -> Integer -> Script
makeBobCommitRedeem aPubkey bPubkey bHashes locktime =
    Script $ [ OP_IF
             -- OP_NOP2 is OP_CHECKLOCKTIMEVERIFY (CLTV)
             -- TODO update haskoin with cltv op
             , opPushData (S.encode locktime), OP_NOP2, OP_DROP
             , opPushData (S.encode bPubkey), OP_CHECKSIG
             , OP_ELSE
             , opPushData (S.encode aPubkey), OP_CHECKSIGVERIFY
             , OP_HASH256
             ] ++
             concat (map (\hash -> [OP_DUP, opPushData (S.encode hash), OP_EQUAL, OP_SWAP]) (init bHashes)) ++
             [ opPushData $ S.encode (last bHashes), OP_EQUAL ] ++
             replicate (length bHashes - 1) OP_BOOLOR ++
             [ OP_ENDIF ]

-- |Takes coins to sign, the data to place in the OP_RETURN and the miner's fee value
makeAdTransaction :: [UTXO] -> [PrvKey] -> ByteString -> SatoshiValue -> Either String T.Tx
makeAdTransaction = undefined

-- |Takes a list of items and serializes so it is ready to be placed in a Tx
makeAdData :: [ByteString] -> ByteString
makeAdData = undefined
