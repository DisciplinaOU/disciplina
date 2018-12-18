module Dscp.Witness.Logic.Tx
    ( -- * Money tx
      createTx
    , signTx
    , createTxw
    , txRelatedAddrs

      -- * Publication tx
    , signPubTx
    , createPublicationTxw
    ) where

import Codec.Serialise (Serialise)

import Dscp.Core
import Dscp.Crypto
import Dscp.Util

----------------------------------------------------------------------------
-- Money tx
----------------------------------------------------------------------------

-- | Form a transaction.
-- You have to ensure that sum of outputs plus fees fit into @maxBound @Coin@.
createTx :: SecretKeyData -> Nonce -> [TxOut] -> Fees -> Tx
createTx sk nonce outs fees =
    let inAcc   = TxInAcc { tiaNonce = nonce, tiaAddr = skAddress sk }
        inValue = leftToPanic $ sumCoins (unFees fees : map txOutValue outs)
        tx      = Tx { txInAcc = inAcc, txInValue = inValue, txOuts = outs }
    in tx

-- | Sign a transaction.
signTx :: SecretKeyData -> Tx -> TxWitnessed
signTx sk tx =
    let signature   = sign (skSecret sk) (toTxId tx, skPublic sk)
        witness     = TxWitness   { txwSig = signature, txwPk = skPublic sk }
        txWitnessed = TxWitnessed { twTx   = tx, twWitness = witness }
    in txWitnessed

-- | Form and sign a transaction.
-- You have to ensure that sum of outputs plus fees fit into @maxBound @Coin@.
createTxw :: FeePolicy Tx -> SecretKeyData -> Nonce -> [TxOut] -> TxWitnessed
createTxw feePolicy sk nonce outs =
    fixFees feePolicy $ \fees ->
        signTx sk $ createTx sk nonce outs fees

-- | Whether transaction is related to the given address.
txRelatedAddrs :: Tx -> [Address]
txRelatedAddrs Tx{..} = tiaAddr txInAcc : map txOutAddr txOuts

----------------------------------------------------------------------------
-- Publication tx
----------------------------------------------------------------------------

-- | Sign publication transaction.
signPubTx
    :: (Serialise PublicationTx, Serialise PrivateBlockHeader)
    => SecretKeyData -> PublicationTx -> PublicationTxWitnessed
signPubTx sk tx =
    PublicationTxWitnessed
    { ptwTx = tx
    , ptwWitness = witness
    }
  where
    pk = skPublic sk
    witness =
        PublicationTxWitness
        { pwSig = sign (skSecret sk) (toPtxId tx, pk, ptHeader tx)
        , pwPk = pk
        }

-- | Wrap private block header into publication transaction.
createPublicationTxw
    :: FeePolicy PublicationTx
    -> SecretKeyData
    -> PrivateBlockHeader
    -> PublicationTxWitnessed
createPublicationTxw feePolicy sk header =
    let tx = PublicationTx
            { ptAuthor = skAddress sk
            , ptFeesAmount = unFees $ calcFeePub feePolicy header
            , ptHeader = header
            }
    in signPubTx sk tx
