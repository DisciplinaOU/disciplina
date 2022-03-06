-- | Validation of public transactions.

module Dscp.Core.Validation.Witness
       ( verifyTxWitnessed
       , verifyPubTxWitnessed
       ) where

import Universum
import Codec.Serialise (Serialise)

import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util

-- | Verify a transaction coupled with witness.
verifyTxWitnessed :: Serialise Tx => TxWitnessed -> Bool
verifyTxWitnessed TxWitnessed {..} =
    let pk = txwPk twWitness
        authorAddr = tiaAddr $ txInAcc twTx
    in mkAddr pk == authorAddr &&
       verify pk (getId twTx, pk, ()) (txwSig twWitness)

-- | Verify a publication coupled with witness.
verifyPubTxWitnessed
    :: (Serialise PrivateBlockHeader, Serialise PublicationTx)
    => PublicationTxWitnessed -> Bool
verifyPubTxWitnessed PublicationTxWitnessed {..} =
    let pk = pwPk ptwWitness
        authorAddr = ptAuthor ptwTx
    in mkAddr pk == authorAddr &&
       verify pk (getId ptwTx, pk, ptHeader ptwTx) (pwSig ptwWitness)
