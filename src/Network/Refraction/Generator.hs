{-# LANGUAGE OverloadedStrings #-}
module Network.Refraction.Generator
    ( makeSimpleTransaction
    , makeAdTransaction
    , makeAliceClaim
    , makeAliceCommit
    , makeBobClaim
    , makeBobCommit
    , makePairRequest
    , makeSplitTransaction
    , mkInput -- TODO do not export this once we're testing the higher level functions
    ) where

import Data.Word (Word64)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import qualified Data.Serialize as S
import Network.Haskoin.Crypto
import Network.Haskoin.Script
import Network.Haskoin.Transaction
import Network.Haskoin.Util (updateIndex)
import Network.Refraction.BitcoinUtils


-- Default transaction fee in satoshis
dTxFee = 10000

-- Mix Unit (0.00090000 BTC for now)
mixUnit = 80000

-- |Takes a UTXO and returns a SigInput that can be used to sign a Tx
mkInput :: UTXO -> Either String SigInput
mkInput utxo = do
    pso <- decodeOutputBS . scriptOutput $ _txOut utxo
    return $ SigInput pso (_outPoint utxo) (SigAll False) Nothing

-- |Our miner fee grows linearly with the number of inputs
calculateAmount :: [UTXO] -> SatoshiValue
calculateAmount = foldl sumVal 0
  where
    sumVal v utxo = v + coinValue utxo - dTxFee

-- |Takes a list of utxos and associated private keys and pays to an address
makeSimpleTransaction :: [UTXO] -> [PrvKey] -> Address -> Either String Tx
makeSimpleTransaction utxos prvkeys addr = do
    tx <- buildTx (map _outPoint utxos) [(addressToOutput addr, calculateAmount utxos)]
    ins <- mapM mkInput utxos
    signTx tx ins prvkeys

-- |Takes a list of utxos and associated private keys and splits the amount into mixUnit outputs
-- the outputs all go to the first private key given, except for the excess amounts, which
-- go to the refundAddr
makeSplitTransaction :: [UTXO] -> [PrvKey] -> Address -> Either String Tx
makeSplitTransaction utxos prvkeys@(p:_) refundAddr = do
  let outAddr = pubKeyAddr $ derivePubKey p
      outputSum = calculateAmount utxos
      refundOut = (addrToBase58 refundAddr, outputSum `mod` mixUnit)
      outs = replicate (fromIntegral $ outputSum `div` mixUnit) (addrToBase58 outAddr, mixUnit)
  tx <- buildAddrTx (map _outPoint utxos) (refundOut:outs)
  ins <- mapM mkInput utxos
  signTx tx ins prvkeys

-- We reverse the hashes here because the redeem script will expect the secrets in reverse order
makeAliceCommit :: [UTXO] -> [PrvKey] -> PubKey -> PubKey -> [Hash256] -> Either String (Tx, Script)
makeAliceCommit utxos prvkeys aPubkey bPubkey bHashes = do
    let redeemScript = makeAliceCommitRedeem aPubkey bPubkey (reverse bHashes) 100000
    tx <- signP2SH utxos prvkeys redeemScript
    return (tx, redeemScript)

makeAliceClaim :: [UTXO] -> [PrvKey] -> Script -> Integer -> PubKey -> Either String Tx
makeAliceClaim utxos prvkeys redeemScript secret aPubkey = do
    let addr = pubKeyAddr aPubkey
    unsigned <- buildTx (map _outPoint utxos) [(addressToOutput addr, calculateAmount utxos)]
    signClaimTx unsigned [secret] 0 redeemScript (head prvkeys)

makeBobCommit :: [UTXO] -> [PrvKey] -> PubKey -> PubKey -> [Hash256] -> Either String (Tx, Script)
makeBobCommit utxos prvkeys aPubkey bPubkey bHashes = do
    let redeemScript = makeBobCommitRedeem aPubkey bPubkey bHashes 100000
    tx <- signP2SH utxos prvkeys redeemScript
    return (tx, redeemScript)

makeBobClaim :: [UTXO] -> [PrvKey] -> Script -> [Integer] -> PubKey -> Either String Tx
makeBobClaim utxos prvkeys redeemScript sums bPubkey = do
    let addr = pubKeyAddr bPubkey
    unsigned <- buildTx (map _outPoint utxos) [(addressToOutput addr, calculateAmount utxos)]
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
scriptAddrNonStd = ScriptAddress . addressHash . S.encode

signP2SH :: [UTXO] -> [PrvKey] -> Script -> Either String Tx
signP2SH utxos prvkeys redeemScript = do
    let scriptOut = addressToOutput $ scriptAddrNonStd redeemScript
    tx <- buildTx (map _outPoint utxos) [(scriptOut, calculateAmount utxos)]
    ins <- mapM mkInput utxos
    signTx tx ins  prvkeys

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
makeAdTransaction :: [UTXO] -- ^ The outputs to spend
                  -> PrvKey -- ^ the key to sign with
                  -> B.ByteString -- ^ the location to encode in the ad
                  -> Word64 -- ^ the ad nonce to encode
                  -> SatoshiValue -- ^ the ad fee
                  -> B.ByteString -- ^ the Refraction identifier
                  -> Either String Tx
makeAdTransaction utxos prvkey loc nonce adFee ref = do
    let ad = B.concat [ref, loc, S.encode nonce]
    tx <- buildTx (map _outPoint utxos) [
        (DataCarrier ad, 0),
        (addressToOutput (pubKeyAddr (derivePubKey prvkey)), calculateAmount utxos)]
    ins <- mapM mkInput utxos
    signTx tx ins [prvkey]

-- |Takes coins to sign, the data to place in the OP_RETURN and the miner's fee value
makePairRequest :: [UTXO] -- ^ The outputs to spend
                -> PrvKey -- ^ the key to sign with
                -> (Word64, Word64) -- ^ the nonce pair to encode
                -> SatoshiValue -- ^ the ad fee
                -> B.ByteString -- ^ the Refraction identifier
                -> Either String Tx
makePairRequest utxos prvkey (aNonce, rNonce) adFee ref = do
    let ad = B.concat [ref, S.encode aNonce, S.encode rNonce]
    tx <- buildTx (map _outPoint utxos) [
        (DataCarrier ad, 0),
        (addressToOutput (pubKeyAddr (derivePubKey prvkey)), calculateAmount utxos)]
    ins <- mapM mkInput utxos
    signTx tx ins [prvkey]
