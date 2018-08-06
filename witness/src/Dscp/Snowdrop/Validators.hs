-- | Validators and block configuration.

module Dscp.Snowdrop.Validators
    ( blkStateConfig
    ) where


import Data.Default (def)
import qualified Data.Map as M
import qualified Snowdrop.Model.Block as SD
import qualified Snowdrop.Model.State.Core as SD
import qualified Snowdrop.Util as SD

import Dscp.Core hiding (PublicationTxWitness)
import Dscp.Crypto (PublicKey, Signature, hash, verify)
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

instance
    SD.VerifySign
        PublicKey
        (Signature (TxId, PublicKey, ()))
        (TxId, PublicKey, ())
  where
    verifySignature = verify

instance
    SD.VerifySign
        PublicKey
        (Signature (PublicationTxId, PublicKey, Publication))
        (PublicationTxId, PublicKey, Publication)
  where
    verifySignature = verify

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
    proj (AddressTxWitness (SD.WithSignature { wsBody = (it, _, _) })) = Just it
    proj  _                                                            = Nothing

-- | We have to implement one part of the Prism ("contains"), bu we
--   can't rebuild Proofs from TxId back.
instance SD.HasReview Proofs TxId where
    inj = error "impossible to implement"

instance SD.HasPrism Proofs PublicationTxId where
    proj (PublicationTxWitness (SD.WithSignature { wsBody = (it, _, _) })) = Just it
    proj  _                                                                = Nothing

-- | Same as TxId.
instance SD.HasReview Proofs PublicationTxId where
    inj = error "impossible to implement"

instance SD.HasGetter Proofs PublicKey where
    gett (AddressTxWitness     (SD.WithSignature { wsBody = (_, it, _) })) = it
    gett (PublicationTxWitness (SD.WithSignature { wsBody = (_, it, _) })) = it

_baseValidator ::
       forall chgAccum.
       SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
_baseValidator =
    validateSimpleMoneyTransfer @(IOCtx chgAccum) <>
    validatePublication @(IOCtx chgAccum)

----------------------------------------------------------------------------
-- Block configuration
----------------------------------------------------------------------------

blkStateConfig ::
       HasWitnessConfig
    => SD.BlkStateConfiguration SHeader SPayload SUndo HeaderHash
                                (SD.ERwComp Exceptions Ids Values (IOCtx chgAccum) chgAccum)
blkStateConfig = simpleBlkStateConfiguration simpleBlkConfiguration validator

-- | Same as inmemoryBlkStateConfiguration, but works for our patched
-- SPayload. DSCP-175
simpleBlkStateConfiguration ::
       SD.BlkConfiguration SHeader SPayload HeaderHash
    -> SD.Validator Exceptions Ids Proofs Values (IOCtx chgAccum)
    -> SD.BlkStateConfiguration SHeader SPayload SUndo HeaderHash
                                (SD.ERwComp Exceptions Ids Values (IOCtx chgAccum) chgAccum)
simpleBlkStateConfiguration cfg vld =
    SD.BlkStateConfiguration {
      bsfConfig = cfg
    , bsfApplyPayload = \(SPayload txs _) -> do
          chg <- either SD.throwLocalError pure $
              foldM SD.mappendChangeSet def $ map SD.txBody txs
          undo <- SD.liftERoComp $ SD.computeUndo chg
          forM_ txs $ \tx -> do
              SD.liftERoComp $ SD.runValidator vld tx
              SD.modifyRwCompChgAccum (SD.txBody tx)
          pure undo
    , bsfApplyUndo = SD.modifyRwCompChgAccum
    , bsfStoreBlund = \blund -> do
          let blockRef = SD.bcBlockRef cfg (SD.blkHeader $ SD.buBlock blund)
          let chg = SD.ChangeSet $ M.singleton (SD.inj $ SD.BlockRef blockRef)
                                               (SD.New $ SD.inj blund)
          SD.modifyRwCompChgAccum chg
    , bsfGetBlund = SD.liftERoComp . SD.queryOne . SD.BlockRef
    , bsfBlockExists = SD.liftERoComp . SD.queryOneExists . SD.BlockRef
    , bsfGetTip =
          SD.liftERoComp (SD.queryOne SD.TipKey) >>=
          maybe (SD.throwLocalError @(SD.BlockStateException Ids) SD.TipNotFound)
                (pure . SD.unTipValue)
    , bsfSetTip = \newTip' -> do
          let newTip = SD.inj $ SD.TipValue newTip'
          let tipChg = \cons -> SD.ChangeSet $ M.singleton (SD.inj SD.TipKey) (cons newTip)
          oldTipMb <- SD.liftERoComp $ SD.queryOne SD.TipKey
          -- TODO check that tip corresponds to blund storage
          case oldTipMb of
              Nothing                            -> SD.modifyRwCompChgAccum $ tipChg SD.New
              Just (_ :: SD.TipValue HeaderHash) -> SD.modifyRwCompChgAccum $ tipChg SD.Upd
    }

simpleBlkConfiguration ::
       HasWitnessConfig
    => SD.BlkConfiguration SHeader SPayload HeaderHash
simpleBlkConfiguration = SD.BlkConfiguration
    { bcBlockRef     = hash
    , bcPrevBlockRef = getPrevHash . hPrevHash
    , bcBlkVerify    = mconcat verifiers
    , bcIsBetterThan = \_ _ -> True
    , bcMaxForkDepth = 0
    }
  where
    GovCommittee com = gcGovernance $ giveL @WitnessConfig

    getPrevHash h
        | h == genesisHash = Nothing
        | otherwise        = Just h

    verifiers :: [SD.BlockIntegrityVerifier SHeader SPayload]
    verifiers =
      [ SD.BIV $ \(SD.Block Header{..} sbody) ->
          verify hIssuer
                 (BlockToSign hDifficulty hSlotId hPrevHash (sPayOrigBody sbody))
                 hSignature
      , SD.BIV $ \(SD.Block sheader _) ->
          committeeOwnsSlot com (mkAddr $ hIssuer sheader) (hSlotId sheader)
      ]
