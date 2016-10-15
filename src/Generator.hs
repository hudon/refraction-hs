{-# LANGUAGE OverloadedStrings #-}
module Generator
    ( makeSimpleTransaction
    , makeAdData
    , makeAdTransaction
    , makeAliceClaim
    , makeAliceCommit
    , makeBobClaim
    , makeBobCommit
    , SatoshiValue
    , UTXO(..)
    ) where

import Data.Word (Word64)
import Data.ByteString (ByteString)
import qualified Data.Serialize as S
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util (updateIndex)

type SatoshiValue = Word64

data UTXO = UTXO {
      _txOut :: TxOut
    , _outPoint :: OutPoint
} deriving (Show)

instance Coin UTXO where
    coinValue =  outValue . _txOut

-- Default transaction fee in satoshis
dTxFee = 10000

-- |Takes a UTXO and returns a SigInput that can be used to sign a Tx
mkInput :: UTXO -> SigInput
mkInput utxo = SigInput pso (_outPoint utxo) (SigAll False) Nothing
  where pso = either undefined id . decodeOutputBS . scriptOutput $ _txOut utxo

-- |Our miner fee grows linearly with the number of inputs
calculateAmount :: [UTXO] -> SatoshiValue
calculateAmount = foldl sumVal 0
  where
    sumVal v utxo = v + coinValue utxo - dTxFee

-- |Takes a list of utxos and associated private keys and pays to an address
makeSimpleTransaction :: [UTXO] -> [PrvKey] -> Address -> Either String Tx
makeSimpleTransaction utxos prvkeys addr =
    let tx = either undefined id $ buildTx (map _outPoint utxos) [(PayPKHash addr, calculateAmount utxos)]
    in signTx tx (map mkInput utxos) prvkeys

-- We reverse the hashes here because the redeem script will expect the secrets in reverse order
makeAliceCommit :: [UTXO] -> [PrvKey] -> PubKey -> PubKey -> [Hash256] -> Either String (Tx, Script)
makeAliceCommit utxos prvkeys aPubkey bPubkey bHashes = do
    let redeemScript = makeAliceCommitRedeem aPubkey bPubkey (reverse bHashes) 100000
    tx <- signP2SH utxos prvkeys redeemScript
    return (tx, redeemScript)

makeAliceClaim :: [UTXO] -> [PrvKey] -> Script -> Integer -> PubKey -> Either String Tx
makeAliceClaim utxos prvkeys redeemScript secret aPubkey = do
    let addr = pubKeyAddr aPubkey
    unsigned <- buildTx (map _outPoint utxos) [(PayPKHash addr, calculateAmount utxos)]
    signClaimTx unsigned [secret] 0 redeemScript (head prvkeys)

makeBobCommit :: [UTXO] -> [PrvKey] -> PubKey -> PubKey -> [Hash256] -> Either String (Tx, Script)
makeBobCommit utxos prvkeys aPubkey bPubkey bHashes = do
    let redeemScript = makeBobCommitRedeem aPubkey bPubkey bHashes 100000
    tx <- signP2SH utxos prvkeys redeemScript
    return (tx, redeemScript)

makeBobClaim :: [UTXO] -> [PrvKey] -> Script -> [Integer] -> PubKey -> Either String Tx
makeBobClaim utxos prvkeys redeemScript sums bPubkey = do
    let addr = pubKeyAddr bPubkey
    unsigned <- buildTx (map _outPoint utxos) [(PayPKHash addr, calculateAmount utxos)]
    signClaimTx unsigned sums 0 redeemScript (head prvkeys)

-- TODO does this need to be in an either?
-- | Sign a single input in a transaction deterministically (RFC-6979).
signClaimTx :: Tx -> [Integer] -> Int -> Script -> PrvKey -> Either String Tx
signClaimTx tx inputData i rdm key = do
    let sh = SigAll False
        sig = TxSignature (signMsg (txSigHash tx rdm i sh) key) sh
        ins = updateIndex i (txIn tx) (addInput sig)
    return $ createTx (txVersion tx) ins (txOut tx) (txLockTime tx)
  where
    addInput sig x = x{ scriptInput = S.encode . si $ encodeSig sig }
    si sig = Script $
        map (opPushData . S.encode) inputData ++ [opPushData sig, OP_0, opPushData $ S.encode rdm]

-- TODO put this is haskoin
scriptAddrNonStd :: Script -> Address
scriptAddrNonStd = ScriptAddress . hash160 . getHash256 . hash256 . S.encode

signP2SH :: [UTXO] -> [PrvKey] -> Script -> Either String Tx
signP2SH utxos prvkeys redeemScript =
    let scriptOut = PayScriptHash (scriptAddrNonStd redeemScript)
        tx = either undefined id $ buildTx (map _outPoint utxos) [(scriptOut, calculateAmount utxos)]
    in signTx tx (map mkInput utxos) prvkeys

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
makeAdTransaction :: [UTXO] -> [PrvKey] -> ByteString -> SatoshiValue -> Either String Tx
makeAdTransaction = undefined

-- |Takes a list of items and serializes so it is ready to be placed in a Tx
makeAdData :: [ByteString] -> ByteString
makeAdData = undefined
