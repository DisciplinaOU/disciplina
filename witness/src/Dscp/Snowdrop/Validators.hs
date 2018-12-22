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
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Mode
import Dscp.Snowdrop.Types
import Dscp.Witness.Config

----------------------------------------------------------------------------
-- Validator
----------------------------------------------------------------------------

validator :: SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validator = _baseValidator

instance SD.HasGetter PublicKey Address where
    gett = Address . hash

instance SD.HasGetter SPayload [SStateTx] where
    gett = sPayStateTxs

validateNothingFor :: SD.StateTxType
                -> SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
validateNothingFor ty = SD.mkValidator ty []

_baseValidator ::
       forall chgAccum.
       SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
_baseValidator = mconcat
    [ validateNothingFor @chgAccum (getStateTxType AccountTxTypeId)
    , validateNothingFor @chgAccum (getStateTxType PublicationTxTypeId)
    , validateNothingFor @chgAccum (getStateTxType BlockMetaTxTypeId)
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
    , bcValidateFork = \_osParams _proposedChain -> True
    -- TODO ^ check that all headers are from slots which preceed current time
    }
  where
    isBetterThan (SD.OldestFirst proposedChain) (SD.OldestFirst currentChain) = length currentChain < length proposedChain

    getPrevHash h
        | h == hPrevHash genesisHeader = Nothing
        | otherwise        = Just h

    verifiers = mempty  -- we validate everything in 'BlockMetaTx' processing.
