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
import Dscp.Snowdrop.Configuration (Exceptions, Ids, Proofs, SHeader, SPayload, SUndo, TxIds,
                                    Values)

----------------------------------------------------------------------------
-- Validator
----------------------------------------------------------------------------

type IOCtx chgAccum = SD.IOCtx chgAccum Ids Values

validator :: SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validator = mempty

-- TODO DSCP-159: below is my futile attempts to integrate account validator.

instance SD.VerifySign PublicKey
                       (Signature (TxId, TxInAcc))
                       (TxId, A.Author (A.AccountId Address)) where
    verifySignature pk (txHash,A.Author{..}) sig =
        verify pk
               (txHash,TxInAcc { tiaAddr = A.unAccountId auAuthorId
                               , tiaNonce = auNonce })
               sig


instance SD.HasReview Proofs (SD.WithSignature PublicKey (Signature (TxId, TxInAcc)) (TxId, A.Author (A.AccountId Address))) where
    inj = error "DSCP-159"
instance SD.HasPrism Proofs (SD.WithSignature PublicKey (Signature (TxId, TxInAcc)) (TxId, A.Author (A.AccountId Address))) where
    proj = error "DSCP-159"

instance SD.HasReview Proofs TxId where
    inj = error "DSCP-159"
instance SD.HasPrism Proofs TxId where
    proj = error "DSCP-159"

instance SD.HasReview Proofs PublicKey where
    inj = error "DSCP-159"
instance SD.HasPrism Proofs PublicKey where
    proj = error "DSCP-159"

instance SD.HasHash (SD.ChangeSet Ids Values) TxId where
    getHash = error "DSCP-159"

_baseValidator ::
       forall chgAccum.
       SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
_baseValidator =
    A.validateSimpleMoneyTransfer @Exceptions @PublicKey @Address @Ids @Proofs @Values  @(IOCtx chgAccum) @(Signature (TxId, TxInAcc)) @TxId @TxIds


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
    { bcBlockRef = hash
    , bcPrevBlockRef = getPrevHash . hPrevHash
    , bcBlkVerify = mconcat verifiers
    , bcIsBetterThan = \_ _ -> True
    , bcMaxForkDepth = 0
    }
  where
    getPrevHash h | h == genesisHash = Nothing
                  | otherwise = Just h

    verifiers :: [SD.BlockIntegrityVerifier SHeader [SD.StateTx id proof value]]
    verifiers =
      [ -- I should get a hash of block body, but i only have SPayload!
        -- verify pk (BlockToSign hDifficulty hPrevHash (hSignature sheader)
        SD.BIV $ \(SD.Block _sheader _sbody) -> True
      , SD.BIV $ \(SD.Block sheader _) -> pk == (hIssuer sheader)
      ]
