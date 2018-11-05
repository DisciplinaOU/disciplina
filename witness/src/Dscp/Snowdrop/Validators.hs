-- | Validators and block configuration.

module Dscp.Snowdrop.Validators
    ( blkStateConfig
    , validator
    ) where

import qualified Snowdrop.Block as SD
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core hiding (PublicationTxWitness)
import Dscp.Crypto (PublicKey, hash, verify)
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Mode
import Dscp.Snowdrop.PublicationValidation
import Dscp.Witness.Config

----------------------------------------------------------------------------
-- Validator
----------------------------------------------------------------------------

validator :: SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validator = _baseValidator

instance SD.HasGetter PublicKey Address where
    gett = Address . hash

instance SD.HasPrism Proofs AddrTxProof where
    proj (AddressTxWitness it) = Just it
    proj  _                    = Nothing

instance SD.HasReview Proofs AddrTxProof where
    inj = AddressTxWitness

instance SD.HasPrism Proofs PublicationTxProof where
    proj (PublicationTxWitness it) = Just it
    proj  _                        = Nothing

instance SD.HasReview Proofs PublicationTxProof where
    inj = PublicationTxWitness

instance SD.HasPrism Proofs TxId where
    proj (AddressTxWitness (SD.wsBody . ppSignedPart -> (it, _, _))) = Just it
    proj  _                                                          = Nothing

-- | We have to implement one part of the Prism ("contains"), bu we
--   can't rebuild Proofs from TxId back.
instance SD.HasReview Proofs TxId where
    inj = error "impossible to implement"

instance SD.HasPrism Proofs PublicationTxId where
    proj (PublicationTxWitness (SD.wsBody . ppSignedPart -> (it, _, _))) = Just it
    proj  _                                                              = Nothing

-- | Same as TxId.
instance SD.HasReview Proofs PublicationTxId where
    inj = error "impossible to implement"

instance SD.HasGetter Proofs PublicKey where
    gett (AddressTxWitness     (SD.wsBody . ppSignedPart -> (_, it, _))) = it
    gett (PublicationTxWitness (SD.wsBody . ppSignedPart -> (_, it, _))) = it

instance SD.HasGetter SPayload [SStateTx] where
    gett = sPayStateTxs

_baseValidator ::
       forall chgAccum.
       SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
_baseValidator =
    validateSimpleMoneyTransfer @(IOCtx chgAccum) <>
    validatePublication         @(IOCtx chgAccum)

----------------------------------------------------------------------------
-- Block configuration
----------------------------------------------------------------------------

blkStateConfig
    :: HasWitnessConfig
    => SD.BlkStateConfiguration SHeader SPayload BlockBody (SD.Undo Ids Values) HeaderHash
                                (SD.ERwComp Exceptions Ids Values (IOCtx chgAccum) chgAccum)
blkStateConfig = SD.inmemoryBlkStateConfiguration simpleBlkConfiguration validator

simpleBlkConfiguration ::
       HasWitnessConfig
    => SD.BlkConfiguration SHeader SPayload HeaderHash
simpleBlkConfiguration = SD.BlkConfiguration
    { bcBlockRef     = SD.CurrentBlockRef . hash
    , bcPrevBlockRef = SD.PrevBlockRef . getPrevHash . hPrevHash
    , bcBlkVerify    = mconcat verifiers
    , bcIsBetterThan = isBetterThan
    , bcMaxForkDepth = 10 -- max possible fork is 10 blocks
    , bcValidateFork = \_osParams _proposedChain -> True
    -- TODO ^ check that all headers are from slots which preceed current time
    }
  where
    isBetterThan (SD.OldestFirst proposedChain) (SD.OldestFirst currentChain) = length currentChain < length proposedChain
    GovCommittee com = gcGovernance $ giveL @WitnessConfig

    getPrevHash h
        | h == hPrevHash genesisHeader = Nothing
        | otherwise        = Just h

    verifiers :: [SD.BlockIntegrityVerifier SHeader SPayload]
    verifiers =
      [ SD.BIV $ \(SD.Block Header{..} (SPayload _ origBodyHash)) ->
          verify hIssuer
                 (BlockToSign hDifficulty hSlotId hPrevHash origBodyHash)
                 hSignature
      , SD.BIV $ \(SD.Block sheader _) ->
          committeeOwnsSlot com (mkAddr $ hIssuer sheader) (hSlotId sheader)
      ]
