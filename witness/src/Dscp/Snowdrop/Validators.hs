-- | Validators and block configuration.

module Dscp.Snowdrop.Validators
    ( blkStateConfig
    , validator
    ) where

import qualified Snowdrop.Block as SD
import qualified Snowdrop.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core hiding (PublicationTxWitness)
import Dscp.Crypto (PublicKey, hash)
import Dscp.Snowdrop.AccountValidation
import Dscp.Snowdrop.BlockMetaValidation
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Mode
import Dscp.Snowdrop.PublicationValidation
import Dscp.Util.Time
import Dscp.Witness.Config

----------------------------------------------------------------------------
-- Validator
----------------------------------------------------------------------------

validator :: SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validator = _baseValidator

instance SD.HasGetter PublicKey Address where
    gett = Address . hash

instance SD.HasPrism Proofs AddrTxProof where
    proj (AddressTxWitnessProof it) = Just it
    proj  _                         = Nothing

instance SD.HasReview Proofs AddrTxProof where
    inj = AddressTxWitnessProof

instance SD.HasPrism Proofs PublicationTxProof where
    proj (PublicationTxWitnessProof it) = Just it
    proj  _                             = Nothing

instance SD.HasReview Proofs PublicationTxProof where
    inj = PublicationTxWitnessProof

instance SD.HasPrism Proofs TxId where
    proj (AddressTxWitnessProof (SD.wsBody . ppSignedPart -> (it, _, _))) = Just it
    proj  _                                                               = Nothing

-- | We have to implement one part of the Prism ("contains"), bu we
--   can't rebuild Proofs from TxId back.
instance SD.HasReview Proofs TxId where
    inj = error "impossible to implement"

instance SD.HasPrism Proofs PublicationTxId where
    proj (PublicationTxWitnessProof
          (SD.wsBody . ppSignedPart -> (it, _, _))) = Just it
    proj  _                                                              = Nothing

-- | Same as TxId.
instance SD.HasReview Proofs PublicationTxId where
    inj = error "impossible to implement"

instance SD.HasGetter Proofs PublicKey where
    gett (AddressTxWitnessProof     (SD.wsBody . ppSignedPart -> (_, it, _))) = it
    gett (PublicationTxWitnessProof (SD.wsBody . ppSignedPart -> (_, it, _))) = it
    gett BlockMetaTxWitnessProof = error "No public key kept for block meta"

instance SD.HasGetter SPayload [SStateTx] where
    gett = sPayStateTxs

_baseValidator ::
       forall chgAccum.
       SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
_baseValidator = mconcat
    [ validateSimpleMoneyTransfer @(IOCtx chgAccum)
    , validatePublication         @(IOCtx chgAccum)
    , validateBlockMeta           @(IOCtx chgAccum)
    ]

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
    , bcBlkVerify    = verifiers
    , bcIsBetterThan = isBetterThan
    , bcMaxForkDepth = 10 -- max possible fork is 10 blocks
    , bcValidateFork = validateFork
    }
  where
    isBetterThan (SD.OldestFirst proposedChain) (SD.OldestFirst currentChain) = length currentChain < length proposedChain

    getPrevHash h
        | h == hPrevHash genesisHeader = Nothing
        | otherwise        = Just h

    verifiers = mempty  -- we validate everything in 'BlockMetaTx' processing.

    -- This makes sure that every header is from a slot preceeding current time
    validateFork osParams (SD.OldestFirst proposedChain) =
        all (\header -> hSlotId header < curTimeSlotId osParams) proposedChain

    curTimeSlotId :: SD.OSParams -> SlotId
    curTimeSlotId = slotFromMcs . utcTimeToMcs . SD.currentTime
