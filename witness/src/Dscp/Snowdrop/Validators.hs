-- | Validators and block configuration.

module Dscp.Snowdrop.Validators
    ( blkStateConfig
    ) where


import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.Execution as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core
import Dscp.Crypto (PublicKey, Signature, hash, verify)
import Dscp.Snowdrop.AccountValidation as A
import Dscp.Snowdrop.Configuration (Exceptions, Ids, Proofs (..), SHeader, SPayload, SUndo, TxIds,
                                    Values)

----------------------------------------------------------------------------
-- Validator
----------------------------------------------------------------------------

type IOCtx chgAccum = SD.IOCtx chgAccum Ids Values

validator :: SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validator = mempty

instance
    SD.VerifySign
        PublicKey
        (Signature (TxId, PublicKey))
        (TxId, PublicKey)
  where
    verifySignature = verify

instance SD.HasGetter PublicKey Address where
    gett = Address . hash

instance SD.HasGetter Proofs (SD.WithSignature PublicKey (Signature (TxId, PublicKey)) (TxId, PublicKey)) where
    gett (AddressTxWitness it) = it

instance SD.HasGetter Proofs TxId where
    gett (AddressTxWitness (SD.WithSignature { wsBody = (it, _) })) = it

instance SD.HasGetter Proofs PublicKey where
    gett (AddressTxWitness (SD.WithSignature { wsBody = (_, it) })) = it

_baseValidator ::
       forall chgAccum.
       SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
_baseValidator =
    A.validateSimpleMoneyTransfer
        @Exceptions
        @PublicKey
        @Address
        @Ids
        @Proofs
        @Values
        @(IOCtx chgAccum)
        @(Signature (TxId, PublicKey))
        @TxId
        @TxIds

----------------------------------------------------------------------------
-- Block configuration
----------------------------------------------------------------------------

blkStateConfig ::
       PublicKey
    -> SD.BlkStateConfiguration SHeader SPayload SUndo HeaderHash
                                (SD.ERwComp Exceptions Ids Values (IOCtx chgAccum) chgAccum)
blkStateConfig pk =
    SD.inmemoryBlkStateConfiguration (simpleBlkConfiguration pk) validator

simpleBlkConfiguration
    :: PublicKey
    -> SD.BlkConfiguration SHeader [SD.StateTx Ids Proofs Values] HeaderHash
simpleBlkConfiguration pk = SD.BlkConfiguration
    { bcBlockRef     = hash
    , bcPrevBlockRef = getPrevHash . hPrevHash
    , bcBlkVerify    = mconcat verifiers
    , bcIsBetterThan = \_ _ -> True
    , bcMaxForkDepth = 0
    }
  where
    getPrevHash h
        | h == genesisHash = Nothing
        | otherwise        = Just h

    verifiers :: [SD.BlockIntegrityVerifier SHeader [SD.StateTx id proof value]]
    verifiers =
      [ -- I should get a hash of block body, but i only have SPayload!
        -- verify pk (BlockToSign hDifficulty hPrevHash (hSignature sheader)
        SD.BIV $ \(SD.Block _sheader _sbody) -> True
      , SD.BIV $ \(SD.Block  sheader _)      -> pk == (hIssuer sheader)
      ]
