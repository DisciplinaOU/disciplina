-- | Publications expansion and validation.
module Dscp.Snowdrop.Expanders.Publication
    ( toProofPublicationTx
    , seqExpandersPublicationTx
    ) where

import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Snowdrop.Core (Expander (..), SeqExpanders (..), StateTxType (..), ValueOp (..), mkDiffCS,
                      queryOne, queryOneExists)
import Snowdrop.Util

import Dscp.Core
import Dscp.Crypto as Dscp
import Dscp.Snowdrop.Configuration
import Dscp.Snowdrop.Expanders.Common
import Dscp.Snowdrop.Storage.Types
import Dscp.Snowdrop.Types

toProofPublicationTx :: (StateTxType, Proofs)
toProofPublicationTx = (getStateTxType PublicationTxTypeId, NoProof)

seqExpandersPublicationTx
    :: Address
    -> Fees
    -> SeqExpanders Exceptions Ids Proofs Values ctx PublicationTxWitnessed
seqExpandersPublicationTx feesReceiverAddr (Fees minFee) =
    SeqExpanders $ one $ Expander
        (Set.fromList [publicationOfPrefix])
        (Set.fromList [accountPrefix, publicationHeadPrefix, publicationOfPrefix,
                       publicationIdsPrefix, privateBlockTxPrefix])
        $ \ptw@PublicationTxWitnessed { ptwTx } -> do
            let PublicationTx{ ptHeader, ptAuthor, ptFeesAmount } = ptwTx
            let ptxId = hash ptwTx
            let phHash = hash ptHeader
            let prevHash = ptHeader ^. pbhPrevBlock
            let (prevHashM :: Maybe PrivateHeaderHash) =
                    prevHash <$ guard (prevHash /= genesisHeaderHash ptAuthor)

            headerWasEarlier <- queryOneExists (PublicationHead phHash)
            let headerIsLast = prevHash == phHash
            when (headerWasEarlier || headerIsLast) $
                throwLocalError PublicationLocalLoop

            let feeAmount = fromIntegral $ coinToInteger ptFeesAmount
            tipM <- fmap @Maybe unLastPublication <$> queryOne (PublicationsOf ptAuthor)
            mFeesReceiver <- queryOne (AccountId feesReceiverAddr)

            unless (prevHashM == tipM) $
                throwLocalError PublicationPrevBlockIsIncorrect
                    { peGivenPrev = prevHashM, peActualTip = tipM }

            let isPaid = ptAuthor /= feesReceiverAddr

            account <- fromMaybe def <$> queryOne (AccountId ptAuthor)

            when (isPaid && feeAmount < coinToInteger minFee) $
                throwLocalError PublicationFeeIsTooLow
                    { peMinimalFee = coinToInteger minFee, peGivenFee = feeAmount }

            when (aBalance account < feeAmount) $
                throwLocalError PublicationCantAffordFee
                    { peFee = feeAmount, peBalance = aBalance account }

            let dIssuer =
                    maybe (New Account { aBalance = feeAmount, aNonce = 0 })
                          (\issuer -> Upd issuer { aBalance = aBalance issuer + feeAmount })
                          mFeesReceiver

            let feesChanges
                    | feeAmount == 0 = []
                    | isPaid =
                          [ AccountId ptAuthor ==>
                                Upd account
                                    -- You can say that `PubOf ==> LastPub`
                                    -- mechanism will prevent double-pub
                                    -- but lets not depend on that impl detail.
                                    { aNonce   = aNonce account + 1
                                    , aBalance = aBalance account - feeAmount
                                    }
                          , AccountId feesReceiverAddr ==> dIssuer ]
                    -- If publication author and fees receiver are the same
                    -- address, we do not transfer any coins.
                    | otherwise = []

            verifyPublicationWitness ptw

            let change = if isJust tipM then Upd else New
            pure $ mkDiffCS $ Map.fromList $
                [ PublicationsOf  ptAuthor ==> change (LastPublication phHash)
                , PublicationHead phHash   ==> New    (PublicationNext prevHashM)
                , PublicationIds  ptxId    ==> New    (PublicationItself ptw)
                , PrivateBlockTx  phHash   ==> New    (PrivateBlockTxVal ptxId)
                ] ++ feesChanges

verifyPublicationWitness :: PublicationTxWitnessed -> Expansion ctx ()
verifyPublicationWitness PublicationTxWitnessed{ ptwWitness = witness, ptwTx = ptx } = do
    let ptxId = toPtxId ptx
    let pk = pwPk witness
    let author = ptAuthor ptx

    unless (verify pk (ptxId, pk, ptHeader ptx) (pwSig witness)) $
        throwLocalError PublicationSignatureIsIncorrect

    unless (mkAddr pk == author) $
        throwLocalError PublicationWitnessMismatchesAuthor
            { peSignerAddress = mkAddr pk, peAuthor = author }
